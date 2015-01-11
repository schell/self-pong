{-# LANGUAGE FlexibleContexts #-}
module Renderer where

import Shader
import Matrix
import Types
import Linear
import Graphics.GL.Core33
import Graphics.GL.Types
import Graphics.UI.GLFW
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Data.Bits
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import System.Directory
import qualified Data.Foldable as F
import qualified Data.Map as M

glFloatSize :: Int
glFloatSize = sizeOf (undefined :: GLfloat)

newRenderSource :: ( Member (State RenderSources) r
                   , SetMember Lift (Lift IO) r)
                => RenderDef -> Eff r RenderSource
newRenderSource rd@(RenderDef fps uniforms) = do
    mSource <- M.lookup rd <$> get
    case mSource of
        Just r -> return r
        Nothing -> do
            r <- lift $ do
                cwd <- getCurrentDirectory

                shaders <- flip mapM fps $ \(fp, shaderType) -> do
                    src <- readFile $ cwd ++ "/" ++ fp
                    shader <- compileShader src shaderType
                    return shader

                program <- compileProgram shaders

                glUseProgram program

                locs <- flip mapM uniforms $ \attr -> do
                    loc <- withCString attr $ glGetUniformLocation program
                    return $ if loc == (-1)
                             then Nothing
                             else Just (attr, loc)
                return $ RenderSource program $ catMaybes locs
            modify $ M.insert rd r
            return r

newPolyRenderer :: ( Member (State RenderSources) r
                   , SetMember Lift (Lift IO) r)
                => Window -> V4 Float -> [Bezier Float] -> [Triangle Float] -> Eff r Renderer
newPolyRenderer window color bs ts = do
    bezr <- newBezRenderer window color bs
    trir <- newTriRenderer window color ts
    let btr = bezr <> trir
        r t = do glClear GL_DEPTH_BUFFER_BIT
                 glEnable GL_STENCIL_TEST
                 glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE
                 glDepthMask GL_FALSE

                 glStencilMask 0xFF
                 glClear GL_STENCIL_BUFFER_BIT

                 glStencilFunc GL_NEVER 0 1
                 glStencilOp GL_INVERT GL_INVERT GL_INVERT

                 (render btr) t

                 glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
                 glDepthMask GL_TRUE
                 glStencilFunc GL_EQUAL 1 1
                 glStencilOp GL_ZERO GL_ZERO GL_ZERO

                 (render btr) t

        c = cleanup btr
    return $ Renderer r c

newBezRenderer :: ( Member (State RenderSources) r
                  , SetMember Lift (Lift IO) r)
               => Window -> V4 Float -> [Bezier Float] -> Eff r Renderer
newBezRenderer window color bs = do
    -- Create a new rendersource (compiled shader program) or retreive an
    -- existing one out of our RenderSources
    let def = RenderDef [("shaders/bezier.vert", GL_VERTEX_SHADER)
                        ,("shaders/bezier.frag", GL_FRAGMENT_SHADER)]
                        ["projection", "modelview"]
    (RenderSource program locs) <- newRenderSource def

    let Just pjloc = lookup "projection" locs
        Just mvloc = lookup "modelview" locs

    lift $ do
        [vao] <- allocaArray 1 $ \ptr -> do
            glGenVertexArrays 1 ptr
            peekArray 1 ptr
        glBindVertexArray vao
        [pbuf, tbuf, cbuf] <- allocaArray 3 $ \ptr -> do
            glGenBuffers 3 ptr
            peekArray 3 ptr

        let ps = concatMap F.toList $ concatMap (\(Bezier _ a b c) -> [a,b,c]) bs :: [GLfloat]
            cs = concatMap F.toList $ take (length ps) $ cycle [color] :: [GLfloat]
            ts = concatMap (\w -> [0, 0, w, 0.5, 0, w, 1, 1, w]) $
                     map (fromBool . bezWoundClockwise) bs :: [GLfloat]
            psize = glFloatSize * length ps
            csize = glFloatSize * length cs
            tsize = glFloatSize * length ts

        glBindBuffer GL_ARRAY_BUFFER pbuf
        withArray ps $ \ptr ->
            glBufferData GL_ARRAY_BUFFER (fromIntegral psize) (castPtr ptr) GL_STATIC_DRAW
        glEnableVertexAttribArray positionLoc
        glVertexAttribPointer positionLoc 2 GL_FLOAT GL_FALSE 0 nullPtr

        glBindBuffer GL_ARRAY_BUFFER cbuf
        withArray cs $ \ptr ->
            glBufferData GL_ARRAY_BUFFER (fromIntegral csize) (castPtr ptr) GL_STATIC_DRAW
        glEnableVertexAttribArray colorLoc
        glVertexAttribPointer colorLoc 4 GL_FLOAT GL_FALSE 0 nullPtr

        glBindBuffer GL_ARRAY_BUFFER tbuf
        withArray ts $ \ptr ->
            glBufferData GL_ARRAY_BUFFER (fromIntegral tsize) (castPtr ptr) GL_STATIC_DRAW
        glEnableVertexAttribArray uvLoc
        glVertexAttribPointer uvLoc 3 GL_FLOAT GL_FALSE 0 nullPtr

        glBindVertexArray 0

        let renderFunction (Transform txy sxy rot) = do
                (pj,mv) <- getPJMV window txy sxy rot
                glUseProgram program
                with pj $ glUniformMatrix4fv pjloc 1 GL_TRUE . castPtr
                with mv $ glUniformMatrix4fv mvloc 1 GL_TRUE . castPtr
                glBindVertexArray vao
                glDrawArrays GL_TRIANGLES 0 $ fromIntegral $ length bs * 3
                err <- glGetError
                when (err /= 0) $ putStrLn $ "Error: " ++ show err
            cleanupFunction = do
                withArray [pbuf, tbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1

        return $ Renderer renderFunction cleanupFunction

newTriRenderer :: ( Member (State RenderSources) r
                  , SetMember Lift (Lift IO) r)
               => Window -> V4 Float -> [Triangle Float] -> Eff r Renderer
newTriRenderer window color ts = do
    let def = RenderDef [("shaders/vert.glsl", GL_VERTEX_SHADER)
                        ,("shaders/frag.glsl", GL_FRAGMENT_SHADER)]
                        ["projection", "modelview"]
    (RenderSource program locs) <- newRenderSource def
    let Just pjloc = lookup "projection" locs
        Just mvloc = lookup "modelview" locs

    lift $ do
        [vao] <- allocaArray 1 $ \ptr -> do
            glGenVertexArrays 1 ptr
            peekArray 1 ptr
        glBindVertexArray vao

        [pbuf,cbuf] <- allocaArray 2 $ \ptr -> do
            glGenBuffers 2 ptr
            peekArray 2 ptr

        let ps = concatMap F.toList $ concatMap (\(Triangle a b c) -> [a,b,c]) ts :: [GLfloat]
            cs = concatMap F.toList $ take (length ps) $ cycle [color] :: [GLfloat]
            pssize = glFloatSize * length ps
            cssize = glFloatSize * length cs

        glBindBuffer GL_ARRAY_BUFFER pbuf
        withArray ps $ \ptr ->
            glBufferData GL_ARRAY_BUFFER (fromIntegral pssize) (castPtr ptr) GL_STATIC_DRAW
        glEnableVertexAttribArray positionLoc
        glVertexAttribPointer positionLoc 2 GL_FLOAT GL_FALSE 0 nullPtr

        glBindBuffer GL_ARRAY_BUFFER cbuf
        withArray cs $ \ptr ->
            glBufferData GL_ARRAY_BUFFER (fromIntegral cssize) (castPtr ptr) GL_STATIC_DRAW
        glEnableVertexAttribArray colorLoc
        glVertexAttribPointer colorLoc 4 GL_FLOAT GL_FALSE 0 nullPtr

        glBindVertexArray 0

        let renderFunction (Transform txy sxy rot) = do
                (pj,mv) <- getPJMV window txy sxy rot
                glUseProgram program
                with pj $ glUniformMatrix4fv pjloc 1 GL_TRUE . castPtr
                with mv $ glUniformMatrix4fv mvloc 1 GL_TRUE . castPtr
                glBindVertexArray vao
                glDrawArrays GL_TRIANGLES 0 $ fromIntegral $ length ts * 3
                err <- glGetError
                when (err /= 0) $ putStrLn $ "Error: " ++ show err
            cleanupFunction = do
                withArray [pbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1

        return $ Renderer renderFunction cleanupFunction

newLineRenderer :: ( Member (State RenderSources) r
                  , SetMember Lift (Lift IO) r)
               => Window -> V4 Float -> [Line Float] -> Eff r Renderer
newLineRenderer window color ls = do
    let def = RenderDef [("shaders/vert.glsl", GL_VERTEX_SHADER)
                        ,("shaders/frag.glsl", GL_FRAGMENT_SHADER)]
                        ["projection", "modelview"]
    (RenderSource program locs) <- newRenderSource def
    let Just pjloc = lookup "projection" locs
        Just mvloc = lookup "modelview" locs

    lift $ do
        [vao] <- allocaArray 1 $ \ptr -> do
            glGenVertexArrays 1 ptr
            peekArray 1 ptr
        glBindVertexArray vao

        [pbuf,cbuf] <- allocaArray 2 $ \ptr -> do
            glGenBuffers 2 ptr
            peekArray 2 ptr

        let ps = concatMap F.toList $ concatMap (\(Line a b) -> [a,b]) ls :: [GLfloat]
            cs = concatMap F.toList $ take (length ps) $ cycle [color] :: [GLfloat]
            pssize = glFloatSize * length ps
            cssize = glFloatSize * length cs

        glBindBuffer GL_ARRAY_BUFFER pbuf
        withArray ps $ \ptr ->
            glBufferData GL_ARRAY_BUFFER (fromIntegral pssize) (castPtr ptr) GL_STATIC_DRAW
        glEnableVertexAttribArray positionLoc
        glVertexAttribPointer positionLoc 2 GL_FLOAT GL_FALSE 0 nullPtr

        glBindBuffer GL_ARRAY_BUFFER cbuf
        withArray cs $ \ptr ->
            glBufferData GL_ARRAY_BUFFER (fromIntegral cssize) (castPtr ptr) GL_STATIC_DRAW
        glEnableVertexAttribArray colorLoc
        glVertexAttribPointer colorLoc 4 GL_FLOAT GL_FALSE 0 nullPtr

        glBindVertexArray 0

        let renderFunction (Transform txy sxy rot) = do
                (pj,mv) <- getPJMV window txy sxy rot
                glUseProgram program
                with pj $ glUniformMatrix4fv pjloc 1 GL_TRUE . castPtr
                with mv $ glUniformMatrix4fv mvloc 1 GL_TRUE . castPtr
                glBindVertexArray vao
                glDrawArrays GL_LINES 0 $ fromIntegral $ length ls * 2
                err <- glGetError
                when (err /= 0) $ putStrLn $ "Error: " ++ show err
            cleanupFunction = do
                withArray [pbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1

        return $ Renderer renderFunction cleanupFunction

getPJMV :: Window -> V2 GLfloat -> V2 GLfloat -> GLfloat -> IO (M44 GLfloat, M44 GLfloat)
getPJMV window (V2 x y) (V2 w h) r = do
    (ww, wh) <- getWindowSize window
    let (hw,hh) = (fromIntegral ww, fromIntegral wh)
        sxy = V3 w h 1
        txy = V3 x y 0
        rxy = V3 0 0 1
        rot = if r /= 0 then mat4Rotate r rxy else eye4
        pj  = ortho 0 hw hh 0 0 1 :: M44 GLfloat
        mv  = (mat4Translate txy) !*! rot !*! (mat4Scale sxy) :: M44 GLfloat
    return (pj,mv)

enableBlending :: IO ()
enableBlending = do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

drawClear :: Window -> IO ()
drawClear window = do
    (fbw, fbh) <- getFramebufferSize window
    glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
    glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
