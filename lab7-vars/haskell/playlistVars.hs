module PlaylistVars where

import Text.Printf

data Product
    = Product
        {
          product_name :: String
        , product_brand :: String
        }
    deriving (Eq)

instance Show Product where 
    show (Product name brand) = 
        printf "%s by %s" name brand

data Item
    = Piece
        {
            item_name :: String,
            item_performer :: String,
            item_length_secs :: Float
        }
    | Advert
        {
            item_product :: Product,
            item_length_secs :: Float
        }
    deriving (Eq)

isAdvert (Advert _ _) = True
isAdvert _ = False

itemOK (Piece _ _ len) = 0 < len && len < 36000
itemOK (Advert _ len) = 0 < len && len < 120

instance (Show Item) where
    show (Piece name performer len) =
        printf "%s by %s (%.1fs)" name performer len
    show (Advert product len) =
        printf "Advert for %s (%.1fs)" (show product) len

playlist1 = [piece1, advert1, piece2]
    where
    piece1  = Piece "Moonlight" "C. Arrau"       (17*minutes+26*seconds)
    piece2  = Piece "Pathetique" "D. Barenboim"  (16*minutes+49*seconds)
    advert1 = Advert (Product "chocolate" "Yummm")          (15*seconds)
    minutes = 60*seconds -- TASK: identify the scope of variable "minutes"
    seconds = 1
    -- Task 7.4a
    -- The scope of variable minutes starts from line 43 after the "="
    -- character starting with "[piece1, advert1, piece2]" on line 43.
    -- The scope of variable minutes ends on line 49 after the "1"
    -- character from the seconds variable.  After the "1" the 
    -- scope is terminated due to no indentation onwards.


    -- ask if we need to mention if it is non-linear static scoped?
        
    -- The scope is non-linear statically scoped which means variables
    -- can be accessed within a scope before they are initialised. The
    -- minutes variable has a scope within the entire of the indented
    -- block of code in playlist1.


length1 = sum [ item_length_secs item | item <- playlist1 ]
    -- OPTIONAL TASK: identify the scope of variable "item" above

main :: IO ()
main =
    do
    printf "playlist1 = %s\n" (show playlist1)
    printf "lenght1 = %s\n" (show length1)

