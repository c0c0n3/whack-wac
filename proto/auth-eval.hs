import Prelude hiding (read)


type Agent    = String
type Resource = String

data Method = GET | HEAD | POST | PUT | PATCH | DELETE deriving (Eq, Show)
read        = [ GET, HEAD ]
write       = [ POST, PUT, PATCH, DELETE ]

type AccessMode = [Method]

data AuthRule = AuthRule {
    subject :: [Agent], mode :: AccessMode, object :: [Resource]
}
type ACLResource = [AuthRule]

data Request = Request {
    user :: Agent, method :: Method, res :: Resource
} deriving Show

ev :: Request -> ACLResource -> Bool
ev req = any rule
    where
    rule p = res req    `elem` object p
          && user req   `elem` subject p
          && method req `elem` mode p


--
-- little extra: EDSL to define ACL resources.
--

can :: [Agent] -> AccessMode -> ([Agent], AccessMode)
can = (,)

(~>) :: ([Agent], AccessMode) -> [Resource] -> AuthRule
(as, ms) ~> rs = AuthRule { subject = as, mode = ms, object = rs }


--
-- examples
--

acl = [ ["u1", "u2"] `can` read ~> ["r1"]
      , ["u2"] `can` write ~> ["r1", "r2"]
      ]

printDecision req = putStrLn (show req) >> putStrLn granted >> putStrLn ""
    where
    decision = ev req acl
    granted  = "granted: " ++ (show decision)

main = do
    printDecision $ Request { user = "u1", method = GET, res = "r1" }
    printDecision $ Request { user = "u1", method = POST, res = "r1" }
    printDecision $ Request { user = "u1", method = GET, res = "r2" }
    printDecision $ Request { user = "u1", method = POST, res = "r2" }
    printDecision $ Request { user = "u2", method = GET, res = "r1" }
    printDecision $ Request { user = "u2", method = POST, res = "r1" }
    printDecision $ Request { user = "u2", method = GET, res = "r2" }
    printDecision $ Request { user = "u2", method = POST, res = "r2" }
    printDecision $ Request { user = "u2", method = GET, res = "unknown" }
