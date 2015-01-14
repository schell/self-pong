{-# LANGUAGE FlexibleContexts #-}
module Renderer where

import Shader
import Matrix
import Types
import Entity
import Font
import Triangulation.KET
import Linear
import Graphics.GL.Core33
import Graphics.GL.Types
import Graphics.UI.GLFW
import Graphics.Text.TrueType
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Data.Bits
import Data.Maybe
import Data.Monoid
import Control.Concurrent.Async
import Control.Applicative
import Control.Monad
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import System.Directory
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.Map as M

drawWithCache :: ( Member (State (IM.IntMap Renderer)) r
                 , SetMember Lift (Lift IO) r)
              => Int -> Eff r Renderer -> Transform -> Eff r ()
drawWithCache uid f tfrm = do
    rcache <- get
    draw <- case IM.lookup uid rcache of
        Just r -> return r
        Nothing -> do r <- f
                      ID uid `hasProperty` r
                      return r
    lift $ render draw tfrm

textDraw :: ( Member (State Globals) r
            , Member (State RenderSources) r
            , SetMember Lift (Lift IO) r)
         => V4 Float -> Font -> Dpi -> PointSize -> String -> Eff r Renderer
textDraw color font dpi psize txt = do
    let cs = getStringCurveAtPoint dpi (0,0) [(font, psize, txt)]
        bs = beziers cs
        bs' = concat $ concat bs
        ts = concatMap (concatMap (concaveTriangles . onContourPoints)) bs
    window <- gWindow <$> get
    newPolyRenderer window color bs' ts

toLines :: [V2 a] -> [Line a]
toLines (a:b:cs) = Line a b : toLines (b:cs)
toLines _ = []

toArrows :: Floating a => [V2 a] -> [Line a]
toArrows = concatMap toArrow . toLines
    where toArrow (Line a b) = [ Line a b
                               , Line (b - u*l + n * w) b
                               , Line (b - u*l + n * (-w)) b ]
            where n = signorm $ perp $ b - a
                  u = signorm $ b - a
                  l = 5 -- head length
                  w = 3 -- head width

drawThing :: ( SetMember Lift (Lift  IO) r
             , Member (State RenderCache) r
             , Member (State RenderSources) r
             , Member (State Globals) r
             , Member (Reader (Async FontCache)) r)
          => Int -> V4 Float -> Display -> Transform -> Eff r ()
drawThing uid color (DisplayAsTris ts) = drawWithCache uid $ do
    window <- gWindow <$> get
    newTriRenderer window color ts
drawThing uid color (DisplayAsPoly vs) = drawWithCache uid $ do
    window <- gWindow <$> get
    newTriRenderer window color $ triangulate vs
drawThing uid color (DisplayAsLine vs) = drawWithCache uid $ do
    window <- gWindow <$> get
    newLineRenderer window color $ toLines vs
drawThing uid color (DisplayAsArrows vs) = drawWithCache uid $ do
    window <- gWindow <$> get
    newLineRenderer window color $ toArrows vs
drawThing uid color (DisplayAsText desc dpi psize txt) = \t ->
    withFont desc $ \font ->
        drawWithCache uid (textDraw color font dpi psize txt) t
drawThing uid color (DisplayAsText' desc dpi psize txt) = \t ->
    withFont desc $ \font ->
        flip (drawWithCache uid) t $ do
            txtr <- textDraw color font dpi psize txt
            let cs = getStringCurveAtPoint dpi (0,0) [(font, psize, txt)]
                bs = beziers cs
                as = concatMap (concatMap toArrows) $ map (map onContourPoints) bs
            window <- gWindow <$> get
            linr <- newLineRenderer window color as
            let r = txtr <> linr
            return r

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
                shaders <- forM fps $ \(fp, shaderType) -> do
                    src <- readFile $ cwd ++ "/" ++ fp
                    compileShader src shaderType
                program <- compileProgram shaders
                glUseProgram program
                locs <- forM uniforms $ \attr -> do
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
        c = cleanup btr
        r t = do glClear GL_DEPTH_BUFFER_BIT
                 glEnable GL_STENCIL_TEST
                 glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE
                 glDepthMask GL_FALSE
                 glStencilMask 0xFF
                 glClear GL_STENCIL_BUFFER_BIT
                 glStencilFunc GL_NEVER 0 1
                 glStencilOp GL_INVERT GL_INVERT GL_INVERT
                 render btr t

                 glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
                 glDepthMask GL_TRUE
                 glStencilFunc GL_EQUAL 1 1
                 glStencilOp GL_ZERO GL_ZERO GL_ZERO
                 render btr t
                 glDisable GL_STENCIL_TEST
    return $ Renderer r c

bufferAttrib :: Storable a => GLuint -> GLint -> GLuint -> [a] -> IO ()
bufferAttrib loc n buf as = do
    let asize = length as * glFloatSize
    glBindBuffer GL_ARRAY_BUFFER buf
    withArray as $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral asize) (castPtr ptr) GL_STATIC_DRAW
    glEnableVertexAttribArray loc
    glVertexAttribPointer loc n GL_FLOAT GL_FALSE 0 nullPtr

bufferPositionAttribs :: Storable a => GLuint -> [a] -> IO ()
bufferPositionAttribs = bufferAttrib positionLoc 2

bufferColorAttribs :: Storable a => GLuint -> [a] -> IO ()
bufferColorAttribs = bufferAttrib colorLoc 4

bufferBezAttribs :: Storable a => GLuint -> [a] -> IO ()
bufferBezAttribs = bufferAttrib uvLoc 3

drawBuffer :: Window -> GLuint -> GLint -> GLint -> GLuint -> GLenum -> GLsizei -> Transform -> IO ()
drawBuffer window program pjloc mvloc vao mode num (Transform txy sxy rot) = do
    (pj,mv) <- getPJMV window txy sxy rot
    glUseProgram program
    with pj $ glUniformMatrix4fv pjloc 1 GL_TRUE . castPtr
    with mv $ glUniformMatrix4fv mvloc 1 GL_TRUE . castPtr
    glBindVertexArray vao
    glDrawArrays mode 0 num
    err <- glGetError
    when (err /= 0) $ putStrLn $ "Error: " ++ show err

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

    lift $ withVAO $ \vao -> withBuffers 3 $ \[pbuf, tbuf, cbuf] -> do
        let ps = concatMap F.toList $ concatMap (\(Bezier _ a b c) -> [a,b,c]) bs :: [GLfloat]
            cs = concatMap F.toList $ replicate (length ps) color :: [GLfloat]
            ts = concatMap (\w -> [0, 0, w, 0.5, 0, w, 1, 1, w]) $
                     map (fromBool . bezWoundClockwise) bs :: [GLfloat]
        bufferPositionAttribs pbuf ps
        bufferColorAttribs cbuf cs
        bufferBezAttribs tbuf ts
        glBindVertexArray 0

        let cleanupFunction = do
                withArray [pbuf, tbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1
            num = fromIntegral $ length bs * 3
            renderFunction = drawBuffer window program pjloc mvloc vao GL_TRIANGLES num
        return $ Renderer renderFunction cleanupFunction

colorRenderSource :: ( Member (State RenderSources) r
                  , SetMember Lift (Lift IO) r)
                  => Eff r (GLuint, GLint, GLint)
colorRenderSource = do
    let def = RenderDef [("shaders/vert.glsl", GL_VERTEX_SHADER)
                        ,("shaders/frag.glsl", GL_FRAGMENT_SHADER)]
                        ["projection", "modelview"]
    (RenderSource program locs) <- newRenderSource def
    let Just pjloc = lookup "projection" locs
        Just mvloc = lookup "modelview" locs
    return (program, pjloc, mvloc)

withVAO :: (GLuint -> IO b) -> IO b
withVAO f = do
    [vao] <- allocaArray 1 $ \ptr -> do
        glGenVertexArrays 1 ptr
        peekArray 1 ptr
    glBindVertexArray vao
    r <- f vao
    glBindVertexArray vao
    return r

withBuffers :: Int -> ([GLuint] -> IO b) -> IO b
withBuffers n f = do
    bufs <- allocaArray n $ \ptr -> do
        glGenBuffers (fromIntegral n) ptr
        peekArray (fromIntegral n) ptr
    f bufs

newTriRenderer :: ( Member (State RenderSources) r
                  , SetMember Lift (Lift IO) r)
               => Window -> V4 Float -> [Triangle Float] -> Eff r Renderer
newTriRenderer window color ts = do
    (program, pjloc, mvloc) <- colorRenderSource
    lift $ withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
        let ps = concatMap F.toList $ concatMap (\(Triangle a b c) -> [a,b,c]) ts :: [GLfloat]
            cs = concatMap F.toList $ replicate (length ps) color :: [GLfloat]
        bufferPositionAttribs pbuf ps
        bufferColorAttribs cbuf cs
        let num = fromIntegral $ length ts * 3
            renderFunction = drawBuffer window program pjloc mvloc vao GL_TRIANGLES num
            cleanupFunction = do
                withArray [pbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1
        return $ Renderer renderFunction cleanupFunction

newLineRenderer :: ( Member (State RenderSources) r
                   , SetMember Lift (Lift IO) r)
                => Window -> V4 Float -> [Line Float] -> Eff r Renderer
newLineRenderer window color ls = do
    (program, pjloc, mvloc) <- colorRenderSource
    lift $ withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
        let ps = concatMap F.toList $ concatMap (\(Line a b) -> [a,b]) ls :: [GLfloat]
            cs = concatMap F.toList $ replicate (length ps) color :: [GLfloat]

        bufferPositionAttribs pbuf ps
        bufferColorAttribs cbuf cs

        glBindVertexArray 0

        let num = fromIntegral $ length ls * 2
            renderFunction = drawBuffer window program pjloc mvloc vao GL_LINES num
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
        mv  = mat4Translate txy !*! rot !*! mat4Scale sxy :: M44 GLfloat
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
