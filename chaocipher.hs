import Data.List

rot 0 xs = xs
rot n xs = (drop n' xs) ++ (take n' xs)
 where n' = mod n (length xs)

data Chao = Chao String String
  deriving (Show,Eq)

--TODO make Chao Eq match under rotation

code f k []     = ([],k)
code f k (x:xs) = (z : zs,k'')
  where
   (zs,k'') = code f k' xs
   (z,k')   = f k x

code_char f k c = (lt !! n,k')
  where
    k'      = permute k n
    Just n  = elemIndex c xt
    (xt,lt) = f k

permute k@(Chao pt ct) n = Chao pt' ct'
  where
    ct'           = cc : (take 12 cs) ++ [cz] ++ (drop 12 cs)
    (cc:cz:cs)    = rot n ct
    pt'           = p1 : p2 : (take 11 ps) ++ [pz] ++ (drop 11 ps)
    (p1:p2:pz:ps) = rot (n+1) pt

--encode

encode = code encode_char

encode_char = code_char plain
  where plain (Chao pt ct) = (pt,ct)

--decode

decode = code decode_char

decode_char = code_char cipher
  where cipher (Chao pt ct) = (ct,pt)

--crack

empty = Chao pt ct
  where 
    ct = take 26 $ repeat ' '
    pt = take 26 $ repeat ' '

crack cs ps = crack' cs ps empty

crack' :: String -> String -> Chao -> [Chao]
crack' [] _ k = [k]
crack' (c:cs) (p:ps) k@(Chao pt ct) = foldl (++) [] $ map (crack' cs ps) further
  where
    further = case (pi,ci) of
      (Nothing,Nothing)                     -> possibles k c p
      (Just n,Nothing) | ((ct !! n) == ' ') -> [permute (Chao pt (replaceIndex n c ct)) n]
                       | otherwise          -> []
      (Nothing,Just n) | ((pt !! n) == ' ') -> [permute (Chao (replaceIndex n p pt) ct) n]
                       | otherwise          -> []
      (Just n,Just m)  | n == m             -> [permute k n]
                       | otherwise          -> []
    pi = elemIndex p pt
    ci = elemIndex c ct

possibles k c p = unique $ [x | Just x <- ms]
  where
    ms = map (possible k c p) $ take 26 [0..]

possible (Chao pt ct) c p n = case (pt !! n,ct !! n) of
    (' ',' ') -> Just $ permute (Chao (replaceIndex n p pt) (replaceIndex n c ct)) n
    otherwise -> Nothing

replaceIndex n c cs = (take n cs) ++ [c] ++ (drop (n+1) cs)

unique xs =                     -- remove duplicates
 let
  r = u xs 0                         -- result list
  u []     _ = []                    -- build result
  u (x:xs) n = if member x r n then u xs n
               else x:(u xs (n+1))
  member e xs     0 = False
  member y (x:xs) n = x==y || member y xs (n-1)
 in r

reverse_crack' [] _ k                       = k
reverse_crack' (c:cs) (p:ps) k@(Chao pt ct) = k' --todo
  where
    k' = (Chao (rot (-1) pt) ct)

rewind k cs = rewind' k (reverse cs)

rewind' k []                  = k
rewind' k@(Chao pt ct) (c:cs) = rewind' k' cs
  where
    k'     = rpermute k n (n-1)
    Just n = elemIndex c ct

rpermute k@(Chao pt ct) n m = Chao pt' ct'
  where
    pt'           = p1:p2:p3:(head pz):(take 11 ps)++(drop 1 pz)
    pz            = drop 11 ps
    (p1:p2:p3:ps) = rot m pt
    ct'           = c1:(head cz'):(take 12 cz)++(drop 1 cz')
    cz'           = drop 12 cz
    (c1:cz)       = rot n ct

--rewind k' ct == k
--  where (k,ct) = encode k cs for any cs assuming k has equality under rotation

test = Chao "PTLNBQDEOYSFAVZKGJRIHWXUMC" "HXUCZVAMDSLKPEFJRIGTWOBNYQ"

test_crack cs = crack e cs
  where (e,_) = encode test cs

--Anything with repeating characters in plaintext or ciphertext will let you find the key easier
--take 2 $ test_crack "WELLWELLWELLWELLWELLWELLWELLWELLWELLWELLWELLWELLWELLABCDFGHIJKLMNOPQRSTUVWXYZ"
--take 2 $ test_crack "WELLWELLWELLWHATDOWEHAVEHERETHEN"
--this second example does not give a full key but only has a single 
--solution making it easy to find a full key
--from. Note these don't return the key but the key 
--after use. Would need to rewind the permutations to get the
--inital key.
--
--(reverse "OAHQGUOWFLIREICELAUQMVKGCLTRYJHMRZNOBRVWZOCDFLTEUZXPOGMJSODMMBHEEBSWTBIVAJUPU")
--(Chao "YVUMNSAPEHRCLTKBFQGOIJDXWZ" "UPRZATINVWFYOJQXGBEKCHLDSM")
--
--In fact we find that just
--take 2 $ test_crack "WELLWELLWELLWHATDOWEHAVE"
--or
--take 2 $ test_crack "HELLOHELLOHELLO"
--
--has enough repeated characters to only have a single solution for a given cipher text.
--
--Still to do: This will only give you a key if you have a know 
--plaintext/cipertext pair. If you have only cipher text
--and you know it to contain some plain text and approximately 
--where it contains it then you would have to play the
--decode forward after you retrieved a partial key guess to see 
--if you could fill in a unique full key and 
--also decode a sensible message.
--
--"A  H W  O     V     TL D E"
--"SUZNE  MIGBLOQ  HA  F RTW "

--encode test "WELLDONEISBETTERTHANWELLSAID"
--("OAHQHCNYNXTSZJRRHJBYHQKSOUJY",
--Chao "JIBMESWKYZXUCOPRTLNHFAGVQD" "YFJBGMTKWNOQXCHIDVALZRSPUE")
--
--
--
--

{--
 
Cracking exhibit 1

> pt <- readFile "exhibit-1.pt.txt"
> ct <- readFile "exhibit-1.ct.txt"
> take 2 $ crack (take 50 $ drop 11998 ct) (drop 11998 pt)

you will get back a single key

> rewind (Chao "I  AE QS  DW PH R GOC NTBL" "QYGWJZSKLHP TVU XANECFOIDM") (take 50 $ drop 11998 ct)

now use the key you get back to propagate forward

> take 2 $ crack' (take 1500 $ drop 11998 ct) (drop 11998 pt) (Chao "A   SHIN PTCGB ODEWQ   RL " "XQCTVIGYJHNAM WZEOUFPDSL K")

the key you get back will have a single space on the left ring put the X in it

> rewind (Chao "EHSGILWRMTCYBXKJQUFPADZNOV" "JSRBYWFXOTDLMGVPUKNQACIZHE") (take 1500 $ drop 11998 ct)

you can now play to the end to find out what the last ?????? in the plain text is

> decode (Chao "AYFZSHINJPTCGBUODEWQVXKRLM" "XQCTVIGYJHNAMBWZEOUFPDSLRK") (drop 11998 ct)

but you cannot as yet rewind to the start because there is a typo in the cipher text.

--}
