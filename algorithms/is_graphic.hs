{-
The algorithm in cpp

bool is_Graphic(vector<int> a_list);

bool is_graphic(vector<int> a_list){

vector<int> b_list;

b_list = order(a_list, descending=true);

int element;

element = b_list.pop(0);

if (element == 0){

    if ((b_list.size() == 0) or (b_list.size() == 1)){

       return true
    }else{
       return false
         }
                 }
int inter_array[element];

int counter;

counter = 0

while(counter != inter_array.size()){
inter_array[counter] = 1;
counter += 1;
}
counter = 0;

int result_array[element];

while(counter != inter_array.size()){

     result_array[counter] = b_list[counter] - inter_array[counter];

                                    }
is_graphic(result_array);

}

-}

-- library declaration --

import Data.List

-- --------------------------

createList :: Int -> [Int]

elementCheck :: Int -> Int -> Bool

elementCheck element list_size
  | element == 0 = sizeCheck
  where
    sizeCheck
      | list_size == 0 = True
      | list_size == 1 = True
      | list_size > 1 = False

is_graphic :: [Int] -> Bool

a_list :: [Int]

size_list :: Int

is_graphic a_list
  reverse sort a_list
  first_element = take 1 a_list
  delete first_element a_list
  size_list = length a_list
  elementCheck first_element size_list
  is_graphic a_list
  -- TODO algo not done yet

-- TODO try with the list = [7,5,5,4,4,4,4,3]
