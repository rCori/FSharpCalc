//F# does not have a built in list reverse so I stole someone elses
let reverseList list = List.fold (fun acc elem -> elem::acc) [] list

//Need a new function to turn a string into a list of ints
let vectorize (input : string) =
    let newinput = input.Split(' ')
    let rec listify (myInput : string[]) (acc : int list) =
        match myInput.Length with
        | 0 -> reverseList acc
        | _ -> try 
                    listify myInput.[1..] (System.Int32.Parse(myInput.[0]) :: acc)
                with | :? System.FormatException ->
                                                    //If the vector doesn't work then the user gets to try again until it works
                                                    //This error handeling conforms to the ABAP standard as Ryan Cori programs do
                                                    printfn "Syntax error: cannot recognize vector please try again"
                                                    let input = System.Console.ReadLine()
                                                    let newinput= input.Split(' ')
                                                    listify newinput []
    //The initial call made by vectorize whose sole mission is to return with an int list, dead or alive...
    //This Summer, Ryan Cori presents: Vectorize Redemption
    listify newinput []



//Parses a user supplied int.Throws an error and asks again if the parse gets a format exception
let rec recieveIntSafe() =
    try
        System.Int32.Parse(System.Console.ReadLine())
    with | :? System.FormatException -> printfn "Sytnax error: cannot regonize input please try again"
                                        recieveIntSafe()

                         
//Recursive fuction for dot products
let rec dotproductrec (listX : int list) (listY : int list) (acc : int) =
        match listX with
        | head :: tail -> dotproductrec listX.Tail listY.Tail ((listX.Head * listY.Head) + acc)
        | [] -> acc


//Helper function for dot products
let Dotproduct (listX : int list) (listY : int list) =
    if (listX.Length = listY.Length) then
        dotproductrec listX listY 0
    else 0


//recursive function for scalar products
let rec scalarproductrec (oldList : int list) (newList : int list) (scale : int) =
    match oldList with
    | head :: tail -> scalarproductrec oldList.Tail ((scale * oldList.Head) :: newList) scale 
    | [] -> reverseList newList


//Helper function to start scalar multipication
let Scalarproduct (listX : int list) (scale : int) =
    scalarproductrec listX [] scale

let rec crossProduct (listX : int list) (listY : int list) = 
    if (listX.Length = 3 && listY.Length = 3) then
        [((listX.[1] * listY.[2]) - (listX.[2] * listY.[1]));
        -((listX.[0] * listY.[2]) - (listX.[2] * listY.[0]));
         ((listX.[0] * listY.[1]) - (listX.[1] * listY.[0]))]
    else
        printfn "One or more of your vectors were not the right size\n"
        printfn "Try the first vector again. Remember only 3 elements"
        let input1 = vectorize(System.Console.ReadLine())
        printfn "Now do the second vector. Again, only three elements"
        let input2 = vectorize(System.Console.ReadLine())
        printfn "Ok trying cross product again."
        crossProduct input1 input2

//Find the equation of a line by two points
//Returns a tuple with two int lists. The first is an origin point and the second is the factor
let rec lineByPoints (listX : int list) (listY : int list) =
    if ((listX.Length = 3) && (listY.Length = 3)) then
        (listX, [(listY.[0] - listX.[0]); (listY.[1] - listX.[1]); (listY.[2] - listX.[2])])
    elif ((listX.Length = 2) && (listY.Length = 2)) then
        (listX, [(listY.[0] - listX.[0]); (listY.[1] - listX.[1])])
    else 
        printfn "Error, incorrect vector lengths"
        printfn "Try the first point again, either two or three dimensions"
        let input1 = vectorize(System.Console.ReadLine())
        printfn "Now do the second vector. Again, only three elements"
        let input2 = vectorize(System.Console.ReadLine())
        printfn "Ok trying cross product again."
        lineByPoints input1 input2

//Helper function for linePointIntersection
let checkPointLine (origin:int list) (eq:int list) (point:int list) =
  //Find where the x-coordinate of the desired point is on the line
  //Some float conversions will be necessary here
  //If the y-coordinate of the line at that point and the desired point's y-coordinate are the same then it's a match
  let (factor : float) = ((float point.[0]) - (float origin.[0]))/ float eq.[0]
  let mutable check = true
  for i = 0 to (eq.Length-1) do
    if (not((factor * (float eq.[i])) + (float origin.[i]) = (float point.[i]))) then
      check <- false
  check
  

//With the equation of a line, check if a point is on that line
let linePointIntersection lineEQ (listY : int list) =
    match lineEQ with
    |((origin : int list),(eq : int list)) -> if (origin.Length = listY.Length) then
                                                checkPointLine origin eq listY
                                              else false              
        
        
//This function will get two strings as input, convert them into int lists, and run Dotproduct() on them
let confirmDotProduct() =
    printfn "Got it, dot product. Enter your first vector"
    let input1 = System.Console.ReadLine()
    let vector1 = vectorize(input1)
    printfn "Now the second"
    let input2 = System.Console.ReadLine()
    let vector2 = vectorize(input2)
    printfn "attempting dot product"
    System.Console.WriteLine(Dotproduct vector1 vector2)



//This fucntion will get a vector and a scalar from the user and do scalarProduct() on them
let confirmScalarProduct() =
    printfn "Got it, scalar product. Enter your vector"
    let input1 = System.Console.ReadLine()
    let vector = vectorize(input1)
    printfn "Now what do you want to multiply this vector by"
    let factor = recieveIntSafe()
    printfn "Attempting scalar product"
    System.Console.WriteLine(Scalarproduct vector factor)


//This fucntion gets two vectors of desirably 3 elemts and does crossProduct() on them
let confirmCrossProduct() =
    printfn "Got it, cross product. Enter your first 3 element vector"
    let input1 = vectorize(System.Console.ReadLine())
    printfn "Now enter your second vector, also containing 3 elements"
    let input2 = vectorize(System.Console.ReadLine())
    printfn "Attempting cross product"
    System.Console.WriteLine(crossProduct input1 input2)

//This function gets two vectors of desirably 3 or 2 elements each 
let rec confirmLineByPoints() =
    printfn "Ok you want the equation of a line. Enter the first point of either two or three dimensions"
    let input1 = vectorize(System.Console.ReadLine())
    printfn "Now enter the second point in the line withthe same number of dimensions as the first"
    let input2 = vectorize(System.Console.ReadLine())
    printfn "Attempting to get an equation of a line from your points"
    let result = lineByPoints input1 input2
    match result with 
    | (a, b) -> printfn "%A + c%A" a b

let rec confirmLinePointIntersection() =
    printfn "Ok you want to see if a point lies on a line"
    printfn "Define your line first by what two points you want on it. First point"
    let linePoint1 = vectorize(System.Console.ReadLine())
    printfn "Second point now"
    let linePoint2 = vectorize(System.Console.ReadLine())
    let result = lineByPoints linePoint1 linePoint2
    printfn "Great now what point do you want to check for"
    let checkPoint = vectorize(System.Console.ReadLine())
    System.Console.WriteLine(linePointIntersection result checkPoint)

    
//Recieves thecommand inputed by the user and does the right function based on it
let confirmCommand (command : string) =
    match command with
    | "dot product" -> confirmDotProduct()
    | "scalar product" -> confirmScalarProduct()
    | "cross product" -> confirmCrossProduct()
    | "equation of a line" -> confirmLineByPoints()
    | "point on a line" -> confirmLinePointIntersection()
    | _ -> printfn "I don't know what you are saying"


//The main execution of the program   
while(true) do
    //introduce user to the program
    printfn "what kind of calculation would you like to do"
    //Get a command from them
    let (command : string) = System.Console.ReadLine()
    //confirm and execute the command
    confirmCommand(command)