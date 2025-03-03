
newtype Void = Void Void

dd :: (a -> b -> c) -> (a -> b) -> a -> c
dd f g a = f a (g a)
-- (a => b => c) => (a => b) => a => c
-- we get : (a => b => c) => ((a => b) => a => c)
-- that mean, (a => b => c) must be true
-- break down, we will get a => (b => c)
-- a is true and (b => c) must be true
-- from that b and c are true
-- we know that a, b, c are true
-- so, (a => b => c) => (a => b) => a => c holds

aa :: (a, b) -> Either a b
aa (a, b) = Right b
-- to proof (a, b) -> Either a b is true
-- I will rewrite to a ^ b => a v b and it must be true
-- proof: assume a ^ b is true
-- so, we get a and b are true
-- next, a v b must be true
-- we know that a, b are true
-- so, (a, b) -> Either a b holds

bb :: (a -> b, a) -> b
bb (f, a) = f a
-- to proof (a -> b, a) -> b
-- I will rewrite to (a => b) ^ a => b
-- proof: assume (a => b) ^ a is true
-- we get (a => b) is true, a is true
-- because a is true b must be true to make (a => b) true
-- so, (a => b) ^ a => b holds

cc :: Either (a -> Void) b -> a -> b
cc (Right b) a = b
cc (Left _) _ = error "Void"
-- Either (a -> Void) b -> a -> b
-- I will rewrite to (a -> ~a) v b => a => b
-- proof: with => associativity we will get (a -> ~a) v b => (a => b)
-- assume (a -> ~a) v b is true
--  case 1: b is true
--      if b is true (a -> ~a) v b is true
--      so, (a => b) is true because b is true there is no way for this predicate to be false
--      for this case, (a -> ~a) v b => a => b holds
--  case 2: (a -> ~a) is true
--      if (a -> ~a) is true
--      we know that a must be false because of (false -> _) is true
--      then, true v b is true
--      next, must proof that (a => b) is true
--      the same reason of (a -> ~a)
--      because a is false and we know that (false -> _) is true
--      so, case 2 (a -> ~a) v b => a => b holds
-- since, both cases hold. (a -> ~a) v b => a => b holds