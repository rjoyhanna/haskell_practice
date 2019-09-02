-- Rachel Hanna hw3

-- myremoveduplicates "abacad" => "bcad"
-- myremoveduplicates [3,2,1,3,2,2,1,1] => [3,2,1]

myremoveduplicates :: (Eq a) => [a] -> [a]
myremoveduplicates inlist
   | null inlist = []
   | elem (head inlist) (tail inlist) = myremoveduplicates (tail inlist)
   | otherwise = (head inlist):myremoveduplicates (tail inlist)

myremoveduplicates_pm :: (Eq a) => [a] -> [a]
myremoveduplicates_pm [] = []
myremoveduplicates_pm (headx:tailx)
   | elem headx tailx = myremoveduplicates_pm tailx
   | otherwise = headx:(myremoveduplicates_pm tailx)

-- myintersection "abc" "bcd" => "bc"
-- myintersection [3,4,2,1] [5,4,1,6,2] => [4,2,1]
-- myintersection [] [1,2,3] => []
-- myintersection "abc" "" => ""

myintersection :: (Eq a) => [a] -> [a] -> [a]
myintersection list1 list2
   | null list1 = []
   | null list2 = []
   | elem (head list1) list2 = (head list1) : myintersection (tail list1) list2
   | otherwise = myintersection (tail list1) list2

myintersection_pm :: (Eq a) => [a] -> [a] -> [a]
myintersection_pm [] _ = []
myintersection_pm _ [] = []
myintersection_pm (headx:tailx) list2
   | elem (headx) list2 = (headx) : myintersection (tailx) list2
   | otherwise = myintersection_pm (tailx) list2

-- mylast "" => ""
-- mylast "b" => "b"
-- mylast "abcd" => "d"
-- mylast [1, 2, 3, 4] => [4]
-- mylast [] => []

mylasthelper :: (Eq a) => [a] -> a -> a
mylasthelper inlist outval
   | null inlist = outval
   | otherwise = (mylasthelper (tail inlist) (head inlist))

mylast :: (Eq a) => [a] -> [a]
mylast inlist
   | null inlist = []
   | otherwise = (mylasthelper (tail inlist) (head inlist)):[]

mylasthelper_pm :: (Eq a) => [a] -> a -> a
mylasthelper_pm [] outval = outval
mylasthelper_pm (headx:tailx) _ = (mylasthelper_pm tailx headx)

mylast_pm :: (Eq a) => [a] -> [a]
mylast_pm [] = []
mylast_pm (headx:tailx) = (mylasthelper_pm tailx headx):[]

-- myreverse "" => ""
-- myreverse "abc" => "cba"
-- myreverse [1, 2, 3] => [3, 2, 1]
-- myreverse [] => []

myreversehelper :: (Eq a) => [a] -> [a] -> [a]
myreversehelper inlist outlist
   | null inlist = outlist
   | otherwise = (myreversehelper (tail inlist) ((head inlist):outlist))

myreverse :: (Eq a) => [a] -> [a]
myreverse inlist = myreversehelper inlist []

myreversehelper_pm :: (Eq a) => [a] -> [a] -> [a]
myreversehelper_pm [] outlist = outlist
myreversehelper_pm (headx:tailx) outlist = (myreversehelper_pm tailx (headx:outlist))

myreverse_pm :: (Eq a) => [a] -> [a]
myreverse_pm inlist = myreversehelper_pm inlist []

-- myreplaceall 3 7 [7,0,7,1,7,2,7] => [3,0,3,1,3,2,3]
-- myreplaceall 'x' 'a' "" => ""
-- myreplaceall 'x' 'a' "abacad" => "xbxcxd"

myreplaceallhelper :: (Eq a) => a -> a -> [a] -> [a] -> [a]
myreplaceallhelper add remove inlist outlist
   | null inlist = outlist
   | (head inlist) == remove = (myreplaceallhelper add remove (tail inlist) (add:outlist))
   | otherwise = (myreplaceallhelper add remove (tail inlist) ((head inlist):outlist))

myreplaceall :: (Eq a) => a -> a -> [a] -> [a]
myreplaceall add remove inlist
   | null inlist = []
   | otherwise = (myreplaceallhelper add add (myreplaceallhelper add remove inlist []) [])


myreplaceallhelper_pm :: (Eq a) => a -> a -> [a] -> [a] -> [a]
myreplaceallhelper_pm _ _ [] outlist = outlist
myreplaceallhelper_pm add remove (headx:tailx) outlist
   | remove == headx = (myreplaceallhelper_pm add remove tailx (add:outlist))
   | otherwise = (myreplaceallhelper_pm add remove tailx (headx:outlist))

myreplaceall_pm :: (Eq a) => a -> a -> [a] -> [a]
myreplaceall_pm _ _ [] = []
myreplaceall_pm add remove inlist = (myreplaceallhelper_pm add add (myreplaceallhelper_pm add remove inlist []) [])
