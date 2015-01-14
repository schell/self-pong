module Pong where

pong = undefined --do
    --let dt = realToFrac $ diffUTCTime t utc
    --    (lx',ly',rx',ry') = case fmap (map (zeroOut . realToFrac)) axes of
    --                            Just (lx:ly:rx:ry:_) -> (lx,ly,rx,ry)
    --                            _ -> (0,0,0,0)

    -- Update the left and right paddles
    --Just (ID lpaddle) <- getEntityBy $ Name "leftPaddle"
    --Just (ID rpaddle) <- getEntityBy $ Name "rightPaddle"
    --let vlp = 200 *^ V2 lx' (-ly')
    --    vrp = 200 *^ V2 rx' (-ry')
    --modify $ IM.adjust (& bodyVelocity_ .~ vlp) lpaddle
    --modify $ IM.adjust (& bodyVelocity_ .~ vrp) rpaddle


    --(bs :: IM.IntMap Body) <- get
    --let bs' = wrapBodies ww wh $ stepBodies dt bs
    --(ts :: IM.IntMap Transform) <- get
    --let tu  = IM.intersectionWith (\tfrm body -> tfrm & tfrmTranslation_ .~ bodyPosition body)
    --                              ts bs'
    --    ts' = tu `IM.union` ts

    -- Update the ball
    --Just (ID ball) <- getEntityBy $ Name "ball"
    --let Just ballBody = IM.lookup ball bs'
    --    rev n = if abs n > 0 then -1.0 else 1
    --    ballBody' = if null ballCollisions then ballBody
    --                else ballBody & bodyVelocity_ *~ (fmap rev $ head ballCollisions)
    --                              & bodyPosition_ -~ head ballCollisions
    --    ballCollisions = catMaybes $ map (\(_, b) -> collideBodyInto b ballBody) $
    --                         (filter ((/= ball) . fst) $ IM.toList bs')

    --put $ IM.insert ball ballBody' bs'
    --put ts'


