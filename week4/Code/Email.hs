module Email(wf, email, Email, unEmail) where

data Email = Email String deriving (Show, Eq)
--- x = Email 123@gmail.com

wf :: Email -> Bool
wf (Email xs) = '@' `elem` xs

email :: String -> Maybe Email
email xs = if '@' `elem` xs then Just (Email xs) else Nothing

unEmail :: Email -> String
unEmail (Email xs) = xs
