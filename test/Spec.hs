import Codegen
import Parser
import Types
import Monads

tests =
  [ "(foo bar)" -- invocation
  , "123" -- int
  , "(.bar foo)" -- method
  , "\"String\"" -- string
  , "(new Array)" -- constructor
  , "(+ 1 2)" -- operator
  , "false" ]

test [] = True
test (x:xs) = do
  case lispParser x of
    Right a ->
      case genTopLevel a of
        "" -> False
        _ -> test xs
    Left a -> False

main :: IO ()
main = test tests |> print
