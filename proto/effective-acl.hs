import Data.Maybe (catMaybes)


type Uri         = String
type AclId       = Int
type ResToAcl    = (Uri, Maybe AclId)
data ResHy       = Node ResToAcl [ResHy]
data RootedResHy = Root (Uri, AclId) [ResHy]

aclPath :: ResHy -> Uri -> [AclId]
aclPath tree uri = catMaybes (acc [] tree)
  where
  acc xs (Node (u, mid) ts) | u == uri  = mid : xs
                            | otherwise = concatMap (acc (mid:xs)) ts

-- assume tree contains resource identified by given URI.
-- if not, return root ACL, but that's irrelevant anyway,
-- since the server will respond w/ a 404
aclResOf :: RootedResHy -> Uri -> AclId
aclResOf (Root (u, i) ts) uri = head (aclPath tree uri)
  where
  tree = Node (u, Just i) ts

hy = Root ("/r0", 0) [r1, r2]
r1 = Node ("/r0/r1", Just 1)
     [ Node ("/r0/r1/r3", Just 2) []
     , Node ("/r0/r1/r4", Nothing) []
     , Node ("/r0/r1/r5", Nothing) []
     ]
r2 = Node ("/r0/r2", Nothing)
     [ Node ("/r0/r2/r6", Just 3)
       [ Node ("/r0/r2/r6/r7", Nothing) []
       , Node ("/r0/r2/r6/r8", Just 4) []
       ]
     ]
ps = [ "/r0"
     , "/r0/r1"
     , "/r0/r1/r3"
     , "/r0/r1/r4"
     , "/r0/r1/r5"
     , "/r0/r2"
     , "/r0/r2/r6"
     , "/r0/r2/r6/r7"
     , "/r0/r2/r6/r8"
     ]
results = zip ps $ map (aclResOf hy) ps

main = mapM (putStrLn . show) results
