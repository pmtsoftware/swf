{-# LANGUAGE DuplicateRecordFields #-}

module Agent where

import Relude

import Ollama

test :: IO ()
test = pure ()

opts :: GenerateOps
opts = defaultGenerateOps
    { modelName = "deepseek-r1"
    , prompt = "Write a short poem about Poland (max 50 words)."
    }

go = generate opts Nothing
