module Task7 where

-- Определить типы подтермов в следующих выражениях (включая тип исходного выражения):
null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]

(\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]

let impl = \x y -> not x || y
 in let isMod2 = \x -> x `mod` 2 == 0
     in let isMod4 = \x -> x `mod` 4 == 0
         in \x -> (isMod4 x) `impl` (isMod2 x)
-- Разрешается выносить подвыражения в блок let или where, чтобы улучшить читабельность кода. Разрешается указать частный тип, который получится из общего после мономорфизации.
