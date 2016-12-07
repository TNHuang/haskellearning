{-
	The k-nearest-neghibors algorithm is an intro machine learning algoritm,
	that require no advance math.

	Typically it require a set of existing m data points with n number of attribute vector,
	and a fixed number k. Any new unknown data point can be classify by finding the average
	classification of k nearest neighbors, with distance being compute as the euclidean
	distance between the two data points. Optionally you can also use squared eclidean distance.

	IE: a car can be represent as a set of (price, year, mileage, number of seats, mile per gallon)
	given a list of cars with these data points, and a number k
	we can determine the approximate prices of a new unknown car by find k nearest data-points.
	then get an average prices of these k neighbor. 
	btw eclidean is calculate as edist a(1..n) b(1..m) = ( (a1-b1)^2 + .. + (am-an)^2 )^(0.2)
-}

-- we'll set setup file later, car data point is represent as [make_year, mpg, kilpmileage, price]
-- un-normalzie and no weighting for now, e-dist is only calculate for non-classification parameter

let unknown_car = [1979, 25, 1250]
let cars = [
	[1982, 30, 1200, 3000],
	[1981, 20, 1300, 2000],
	[1983, 10, 1500, 1000],
	[1985, 5, 1100, 1000],
	[2011, 60, 120, 10000],
	[2010, 70, 130, 10000],
	[2015 80, 140, 10000]
];

let k = 4;

-- assume data points of equal length list, classification point is assume at last index
sum_of_square :: (Num a) => [a] -> [a] -> a -> [a];
sum_of_square [] _ = 0;
sum_of_square _ [] = 0;
sum_of_square new_pt existing_pt = [ (new_pt !! i - existing_pt !! i)^2 | i <- [0..length new_pt-2]];

e_distance :: (Double a) => [a] -> [a] -> a;
e_distance [] _ = 0;
e_distance _ [] = 0;
e_distance new_pt existing_pt = sqrt ( sum_of_square new_pt existing_pt );

-- sample quick sort
quick_sort :: (Ord a) => [a] -> [a];
quick_sort [] = [];
quick_sort (x:xs) = quick_sort [ lower | lower <- xs, lower <= x] ++ [x] ++ quick_sort [higher | higher <- xs, higher >= x];

-- tuple sort -> sort by second key, how use the prelude sortBy?? xs = sortBy (comparing snd) xs?
tuple_qs :: (Ord b) => [([a], b)] -> [([a], b)];
tuple_qs [] = [];
tuple_qs (x:xs) = [ lower | lower <- xs, (snd lower) <= (snd x) ] ++ [x] ++ [ higher | higher <- xs, (snd higher) >= (snd x) ];

--create data points tuple (data_pt_list, dist from new pt)
create_dist_tuple :: [a] -> [a] -> ([a], b);
create_dist_tuple [] _ = "no empty new_pt allow";
create_dist_tuple _ [] = "no empty existing_pt allow";
create_dist_tuple new_pt existing_pt = (existing_pt, (e_distance new_pt existing_pt) );

-- create sorted distance tuple list
let sort_dist_tuple_list = tuple_qs [ create_dist_tuple unknown_car existing_car | existing_car <- cars ];

-- take first k values, since it's sorted, take k list would give k nearest neighbors
let k_neighbors = take k sort_dist_tuple_list;

-- compute aprroximate price of the unknown_car, assume price is in last indx
compute_avg_price :: (Num p) => [([a], d)] -> p;
compute_avg_price [] = 0;
compute_avg_price xs = 
	let total = sum [ last (fst x) | x <- xs ]
		len = length xs
	in total/len; 