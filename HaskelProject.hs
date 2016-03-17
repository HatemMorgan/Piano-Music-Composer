import MusicResources
targets a []=[]
targets a [x]=[]
targets a (x:y:xs) = if a==x then y:targets a xs 
                     else targets a (y:xs)
remove a []=[]
remove a (x:xs)= if a==x then remove a xs
                 else [x]++(remove a xs) 
count a []=0
count a (x:xs)= if a==x then 1+ count a xs  
                else count a xs
pairs []=[]
pairs (x:xs)= ((count x (x:xs)),x):pairs (remove x (x:xs)) 

sortedToFrequency  []=[]
sortedToFrequency  (x:xs)= (max1 (x:xs) (0,'0')):sortedToFrequency (removeMaxPair (x:xs) (max1 (x:xs) (0,'0')))   

removeMaxPair [] a=[]
removeMaxPair (x:xs) a = if x==a then removeMaxPair xs a 
                        else x:removeMaxPair xs a

max1 [] a=a
max1 (x:xs) a = if y>(getFrequency a) then max1 xs x  
               else max1 xs a
               where y= getFrequency x


final []=[]
final (x:xs)= (x,(sortedToFrequency (pairs (targets x (concat training))))):(final xs)    

makeStatsList :: [(Char,[(Integer,Char)])]
makeStatsList= final chars 

getFrequency (a,b)=a
getChar1 (a,l)=a
getList1 (a,l)= l  
getSubChar (x,y)=y


getValue [] n=error "this character cannot compose because it cannot get the next character so please choose a new character"
getValue (x:xs) 0=x
getValue (x:xs) n= getValue xs (n-1)  

getSignalList c 0=[]
getSignalList c n= c:getSignalList c (n-1) 

getOurList a (x:xs)= if a==(getChar1 x) then getList1 x 
                     else getOurList a xs  

totalFrequencies []=0
totalFrequencies (x:xs)= (getFrequency x) + totalFrequencies xs

getTotalCharList []=[]
getTotalCharList (x:xs)= (getSignalList (getSubChar x) (getFrequency x))++(getTotalCharList xs) 


getRandomChar c = (getValue (getTotalCharList (getOurList c (makeStatsList))) ( randomZeroToX (totalFrequencies (getOurList c (makeStatsList)))))

composeHelp c 0=[]
composeHelp c n= y:composeHelp y (n-1) where y=(getRandomChar c) 

compose :: Char -> Int-> [Char]
compose c n= c:composeHelp c (n-1)






