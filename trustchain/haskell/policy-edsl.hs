--
-- Toy EDSL to express security policies in a language close to plain
-- English. E.g.
--   role admin can read or can write
--   or anyone who has name (== "joe"), has dob (> 1995) can do anything
--
-- Run the example with the command below
--
--   $ ghc --run policy-edsl.hs
--


import Prelude hiding (or, read, (/))
infixr 0 `or`


--
-- Functionality to represent policy input (e.g. HTTP request)
-- as JSON data.
--

type Name = String
type Path = [Name]
data Json = Nil
          | Str String
          | Nbr Float
          | Bln Bool
          | Arr [Json]
          | Obj [(Name, Json)]
  deriving (Eq, Show)

-- Extract the sub-tree (if any) rooted at the last element of the
-- input path.
at :: Path -> Json -> Json
at []      js                            = js
at (n:ns) (Obj ((m, js):fs)) | n == m    = at ns js
                             | otherwise = at (n:ns) (Obj fs)
at  _      _                             = Nil


--
-- Core policy EDSL.
--

-- Essential building block.
type Policy = Json -> Bool

-- Combine building blocks through logical AND.
(?) :: Policy -> Policy -> Policy
p ? q = \js -> p js && q js

-- Combine building blocks through logical OR.
or :: Policy -> Policy -> Policy
p `or` q = \js -> p js || q js

-- Assert a prop.
has :: Path -> (String -> Bool) -> Policy
has ps cond = check . at ps
  where
  check (Str v) = cond v
  check _       = False

--
-- Policy EDSL syntactic sugar.
--

anyone :: Policy
anyone _ = True

(/) :: Path -> String -> Path
ps / p = ps ++ [p]

doc = []

role r = has (doc/"role") (== r)
can op = has (doc/"op") (== op)


--
-- Example policy.
--

admin = "admin"
read  = "read"
write = "write"
name  = doc/"person"/"name"
dob   = doc/"person"/"dob"

policy =  role admin ? can read
                  `or` can write
       `or`
          anyone ? has name (== "joe")
                 ? has dob  (> "1995")

--
-- Example input
--

input = Obj
  [ ("op", Str "read")
  , ("role", Str "admin")
  , ("person", Obj
      [ ("name", Str "joe")
      , ("dob", Str "1996")
      , ("refs", Arr [Nbr 1, Nbr 2])
      ]
    )
  , ("owner", Bln False)
  , ("other", Arr [Nil, Obj []])
  ]

--
-- Example policy evaluation
--

main = do
  putStrLn . show $ at (doc/"person"/"refs") input
  putStrLn . show $ policy input
-- prints:
--   Arr [Nbr 1.0,Nbr 2.0]
--   True
