{-        addCommands $ do
            helpCommand
            command @'[] "bye" $ \ctx -> do
                --void $ tell ctx (show $ either (\_ -> "wrong") (\c -> c ^. (#name)) (matching _GuildChannel' (ctx ^. (#channel))) :: Text)
                --void $ tell ctx (show $ (is _GuildChannel' (ctx ^. (#channel))) :: Text)
                void $ tell ctx (show $ (^. #name) <$> (matching _GuildChannel' (ctx ^. (#channel))) :: Text)
                void $ tell ctx ("bye!" :: Text)
                stopBot
        --react @'MessageCreateEvt $ \msg -> print $ "Got message: " <> show msg

    --runBotIO (BotToken "NzI2NDI5NTkxMzkyMjIzMjcz.XvdKXQ.JlrRp3GLD1Q2qHNdhjy4GcWSwD8") $ 
        -}