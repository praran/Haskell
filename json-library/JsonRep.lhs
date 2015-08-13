Representation of Json values :
====================================

> import Data.List
> import GHC.Real
> import Text.PrettyPrint.HughesPJ



> data JValue = JString String
>              | JNumber Double
>              | JObject [(String,JValue)]
>              | JArray  [JValue]
>              | JTrue
>              | JFalse
>              | JNull deriving (Show)


 JSon value in haskell
=======================================

(JObject [ ("firstname" , JString "praran"), 
           ("lastname", JString "lastname"), 
		   ("phone", JArray [JString "073131313131", JString "0201231231231231"]) ,
		   ("address",(JObject [ 
            		   ("town", JString "london") , 
					   ("postcode", JString "WC1")]))] )


 Function which converts JValue to String representation
=====================================================================

> jprint :: JValue -> String
> jprint  (JString s)     = show s
> jprint  (JNumber d)     = show d
> jprint  JTrue           = "true"
> jprint  JFalse          = "false"
> jprint  JNull           = "null"
> jprint  (JArray arr)    = "[ " ++ arrToStr arr ++ " ]"
>  where  arrToStr []     = ""
>         arrToStr xs     = foldr (++) [] $ insertSeperator " , " $ map jprint xs 
> jprint  (JObject arr)   = "{ " ++ arrPairToStr arr ++ " }"
>  where  arrPairToStr [] = ""
>         arrPairToStr xs = foldr (++) [] $ insertSeperator " , " $ map  (pairToString) xs 
>         pairToString (k,v) =  k ++ " : " ++ (jprint v)


> insertSeperator :: a -> [a] -> [a]
> insertSeperator _ []        = []
> insertSeperator _ (x:[])    = [x]
> insertSeperator s (x:xs)    = x: s : insertSeperator s xs


pretty print of Json object
=========================================

The solution is achieved using Text.PrettyPrint.HughesPJ library
 
> jpretty :: JValue -> String
> jpretty x = renderStyle (Style {mode=ZigZagMode, lineLength=20, ribbonsPerLine=3}) (jvaltodoc x)

> jvaltodoc :: JValue      -> Doc
> jvaltodoc JTrue          = text "true"
> jvaltodoc JFalse         = text "false"
> jvaltodoc JNull          = text "null"
> jvaltodoc (JNumber d)    = double d
> jvaltodoc (JString s)    = ptext s
> jvaltodoc (JArray xs)    = brackets $ cat $ punctuate comma $  map jvaltodoc xs
> jvaltodoc (JObject vals) = braces $ fsep $ punctuate comma $ map transform vals
>   where transform (k,v)  = text k <+> colon <+> jvaltodoc v


Function jlookup that searches for the frist object in a given value that contains a given name, and returns the associated value
===================================================================================================================

> data  Result a  = Result a | Error String
>   deriving (Eq, Show)

> jlookup :: JValue -> String -> Result JValue
> jlookup  (JString s)  _    = Error "Function jlookup can only be applied to JValue type JObject [(String,JValue)] !!!"
> jlookup  (JNumber d)  _    = Error "Function jlookup can only be applied to JValue type JObject [(String,JValue)] !!!"
> jlookup  JTrue        _    = Error "Function jlookup can only be applied to JValue type JObject [(String,JValue)] !!!"
> jlookup  JFalse       _    = Error "Function jlookup can only be applied to JValue type JObject [(String,JValue)] !!!"
> jlookup  (JArray xs)  _    = Error "Function jlookup can only be applied to JValue type JObject [(String,JValue)] !!!"
> jlookup  (JObject xs) k    = findElem k xs
>    where findElem _ []         = Error "No corresponding value for the supplied key in the given JValue !!!"
>          findElem s ((k,v):ps) 
>            | s == k    = Result v
>            | otherwise = findElem s ps 

Functions to convert haskel types to JValue
=======================================================

> data Person = P { firstname :: String, lastname :: String, age :: Int } 
>  deriving (Show,Read)

> jperson :: Person   -> JValue
> jperson   (P f l a) =  JObject [("firstname",JString f), ("lastname", JString l),("age",JNumber $ fromIntegral a)]
 
> jstring :: String -> JValue
> jstring     =  JString   
 
> jstrings  :: [String] -> JValue
> jstrings  xs  = JArray  $ map jstring xs

> jnumber   :: Double     -> JValue
> jnumber    = JNumber

> jnumbers  :: [Double]   -> JValue
> jnumbers  xs  = JArray $ map jnumber xs

> jnumberss :: [[Double]] -> JValue
> jnumberss xs  = JArray $ map jnumbers xs

[Optional] Capturing convertion of [String] or [Double] as JValue

> arrtojvalue ::(a -> JValue) -> [a] -> JValue
> arrtojvalue f xs  = JArray  $ map f xs

> jstrings'  :: [String] -> JValue
> jstrings'              = arrtojvalue jstring

> jnumbers'  :: [Double] -> JValue
> jnumbers'              = arrtojvalue jnumber


Class JSON to convert Haskell values into Json representation
==============================================================

> class Json a where
>  asJson :: a   -> JValue 

Json instance for boolean value
---------------------------------

> instance Json Bool where
>  asJson b = if b then JTrue else JFalse

Json instance from int value
------------------------------------

> instance Json Int where
>  asJson = JNumber . fromIntegral

Json instance for double value
--------------------------------
 
> instance Json Double where
>  asJson d  = JNumber d

Json instance for JValue type
-----------------------------------

> instance Json JValue where
>  asJson x = x

Json representation for String types
-------------------------------------

> newtype JSString  = JS String deriving Show

> instance Json JSString where
>   asJson  (JS s) = JString s

Json respresentation for array types
-------------------------------------

> newtype JsonArray a = JSArray [a] deriving (Show)

> jsonarray :: [a] -> JsonArray a
> jsonarray = JSArray

> jsonArrayToJValue :: (Json a) => JsonArray a -> JValue
> jsonArrayToJValue (JSArray x) = JArray  $ map asJson x

> instance (Json a) => Json (JsonArray a) where
>   asJson  = jsonArrayToJValue

Json representation for JObject haskell values
-----------------------------------------------------

> newtype  JsonObject a = JSObject [(String,a)]  deriving (Show)

> jsobjectToJValue  ::(Json a) => JsonObject a -> JValue
> jsobjectToJValue (JSObject vs) = JObject $ map pairToJValue vs
>   where pairToJValue (k,v) = (k, asJson v) 

> instance Json a => Json (JsonObject a) where 
>   asJson  = jsobjectToJValue

Instance declaration of EQ for JValue type
================================================

> instance Eq JValue where
>   JTrue     == JTrue       = True
>   JTrue     == _           = False
>   JFalse    == JFalse      = True
>   JFalse    == _           = False
>   JNumber a == JNumber b   = a == b
>   JNumber a == _           = False
>   JString a == JString b   = a == b
>   JString a == _           = False
>   JArray  a == JArray b    = arrayDiff a b == []
>   JArray  a == _           = False
>   JObject a == JObject b   = equalityOfJObject (JSObject a) (JSObject b)
>   JObject a == _           = False  
>   a        /= b            = not (a == b)

Equality of JObjects
----------------------

> equalityOfJObject ::  JsonObject JValue  -> JsonObject JValue -> Bool
> equalityOfJObject (JSObject xs) (JSObject ys) = foldr (&&) True (function xs ys )
>   where  function [] [] =  [True]
>          function [] ys =  [False]
>          function xs [] =  [False]
>          function ((k,v):vs) ys 
>             | (Result v) == (jlookup (JObject ys) k) = True : (function vs (delete (k,v) ys) )
>             |  otherwise = [False]

Find the difference of unordered lists
----------------------------------------

> arrayDiff :: (Eq a) => [a] -> [a] -> [a]
> arrayDiff  []   []    = []
> arrayDiff  []    b    = b
> arrayDiff  a     []   = a
> arrayDiff  (x:xs) b   = arrayDiff xs (delete x b)

-------------------------------------------------------------------------



