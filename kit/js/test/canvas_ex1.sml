 signature CANVAS = sig
  type t
  type elem = Js.elem
  val getContext : elem -> string -> t
  val fillRect   : t -> int -> int -> int -> int -> unit
  val clearRect  : t -> int -> int -> int -> int -> unit
  val beginPath  : t -> unit
  val moveTo     : t -> int -> int -> unit
  val lineTo     : t -> int -> int -> unit
  val closePath  : t -> unit
  val stroke     : t -> unit
  val fillStyle  : t -> string -> unit
  val lineWidth  : t -> string -> unit
  val strokeStyle : t -> string -> unit
end

structure Canvas : CANVAS = struct
  type t = foreignptr
  type elem = Js.elem
  fun getContext e s =
      let val e = Js.Element.toForeignPtr e
      in JsCore.exec2 {stmt="return e.getContext(s);", res=JsCore.fptr,
                       arg1=("e",JsCore.fptr),arg2=("s",JsCore.string)} (e, s)
      end
  fun fillRect c a1 a2 a3 a4 =
      JsCore.exec5 {stmt="c.fillRect(a1,a2,a3,a4);",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("a1",JsCore.int),
                    arg3=("a2",JsCore.int),
                    arg4=("a3",JsCore.int),
                    arg5=("a4",JsCore.int)} (c, a1, a2, a3, a4)
  fun clearRect c a1 a2 a3 a4 =
      JsCore.exec5 {stmt="c.clearRect(a1,a2,a3,a4);",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("a1",JsCore.int),
                    arg3=("a2",JsCore.int),
                    arg4=("a3",JsCore.int),
                    arg5=("a4",JsCore.int)} (c, a1, a2, a3, a4)
  fun beginPath c =
      JsCore.exec1 {stmt="c.beginPath();",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr)} c
  fun closePath c =
      JsCore.exec1 {stmt="c.closePath();",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr)} c
  fun stroke c =
      JsCore.exec1 {stmt="c.stroke();",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr)} c
  fun moveTo c a1 a2 =
      JsCore.exec3 {stmt="c.moveTo(a1,a2);",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("a1",JsCore.int),
                    arg3=("a2",JsCore.int)} (c, a1, a2)
  fun lineTo c a1 a2 =
      JsCore.exec3 {stmt="c.lineTo(a1,a2);",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("a1",JsCore.int),
                    arg3=("a2",JsCore.int)} (c, a1, a2)
  fun fillStyle c s =
      JsCore.exec2 {stmt="c.fillStyle = s;",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("s",JsCore.string)} (c, s)
  fun lineWidth c s =
      JsCore.exec2 {stmt="c.lineWidth = s;",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("s",JsCore.string)} (c, s)
  fun strokeStyle c s =
      JsCore.exec2 {stmt="c.strokeStyle = s;",
                    res=JsCore.unit,
                    arg1=("c",JsCore.fptr),
                    arg2=("s",JsCore.string)} (c, s)
end

local

val screenWidth    = 400
val screenHeight   = 200

fun ppInt i = if i < 0 then "-" ^ Int.toString (~i) else Int.toString i

fun $ id =
    case Js.getElementById Js.document id of 
      SOME e => e
    | NONE => raise Fail ("no element with id '"^id^"' in DOM")
    
fun println s = print (s ^ "\n")
val _ = println "<html><head>"
val _ = println "<title>Canvas example</title>"
val _ = println "<style>"
val _ = println "div#minimapcontainer { }"
val _ = println "canvas#minimap { position : absolute; }"
val _ = println "canvas#minimapobjects { position : absolute; }"
val _ = println "div#floor { position : absolute; width : 100%; height : 100%; background-color : rgb(128,128,128); z-index : -10000000; }"
val _ = println "div#ceiling { position : absolute; width : 100%; height : 50%; background-color : rgb(96,96,96); z-index : -10000000; }"
val _ = println ("#screen { position : relative; width : " ^ ppInt screenWidth ^ "px; height : " ^ ppInt screenHeight ^ "px; border : 1px solid black; overflow : hidden; }")
val _ = println ("div#overlay { position : absolute; display : block; width : " ^ 
                 ppInt (screenWidth - 10) ^ "px; height : " ^ ppInt (screenHeight - 10) ^ 
                 "px; padding : 5px; color : white; font-family : lucida console, courier new; font-size : 20px; z-index : 1; }")
val _ = println ("div#score { position : relative; display : block; width : " ^ 
                 ppInt (screenWidth - 10) ^ "px; height : " ^ ppInt 30 ^ 
                 "px; padding : 5px; color : blue; font-family : lucida console, courier new; font-size : 20px; z-index : 1; }")
val _ = println "</style>"
val _ = println "</head><body>"
val _ = println "<div id='screen'>"
val _ = println "<div id='floor'></div>"
val _ = println "<div id='ceiling'></div>"
val _ = println "<div id='overlay'></div>"
val _ = println "</div>"
val _ = println "<div id='score'> </div>"
val _ = println "<div id='minimapcontainer'>"
val _ = println "<canvas id='minimap'></canvas>"
val _ = println "<canvas id='minimapobjects'></canvas>"
val _ = println "</div>"
val _ = println "<div id='log'></div>"
val _ = println "</body></html>"

fun log s = let val log = $"log"
            in Js.appendChild log (Js.createTextNode s);
               Js.appendChild log (Js.createElement "br")
            end

structure MapObj = struct
  val O = 0
  val W = 1
  val w = 2
  val m = 3
  val H = 4
  val T = 100
  val A = 101
  val P = 102
  val L = 103
  val B = 104
  val C = 105
  fun isSprite n = n >= 100
end

val Smap0 = [
    "***************************",
    "*   %          %   %   O  %",
    "*         T        %   L  %",
    "*   %          %   %      %",
    "*% %%%%%%%%%%%%%   %%%%% %%",
    "*      %P     P%      %   %",
    "*      %       %      %   %",
    "*      %% %%% %%   L      %",
    "***** **   *   *      *   *",
    "*          *   ************",
    "* L        *              *",
    "*          *           A  *",
    "%%%%%%%%%%%%%             *",
    "%           %%%% %%%%     %",
    "*   A         *     *     *",
    "*             *     *******",
    "*         T   *     *     *",
    "*             *     *     *",
    "*             *     *     *",
    "*    T        *     *     *",        
    "*             *     **** **",
    "*             *           *",
    "** ************       T   *",
    "*                         *",
    "*                  T    L *",
    "*                         *",
    "***************************"
]

val Smap = [
    "*********************************H*H***H*H*%%%%%%%%%%%%%%%%%%HHHHHH%%%%%%%%HHHHHHHH%%%%%%HHHHHHH%%%%%%HHHHHHH**",
    "*         T   T              L *                   %                         %              %                 *",
    "*  %%%%                        *            L   O  %    P  P                 %              %                 *",
    "*  %  %  **O*****O***O*        *                   %                                                A  A      *",
    "*  %  H                        *           %%%%%%%%%    P  P                                                  *",
    "*     %             P          *                   %                                                A  A      *",
    "*     %    *****          %**********              *    P  P                    L                             *",
    "*  %  %                        *                   *                       L         L              A  A      *",
    "*  %  H                     L     L       **       *                                                          *",
    "*  %  %       A                *                   *                     L      A      L            A  A      *",
    "*  %%%%              P    %*****                   *                                                          *",
    "*                              *                   *                       L         L                        *",
    "*        %%%  %%%              O    L  L  L  L  L  *%%%%                        L                             *",
    "*        %%%  %%%              *                   *                                                       %%%*",
    "*    L   %%%  %%%         m***** P                 *                                                          *",
    "*        %%%  %%%              *        H%H        *                                                          *",
    "*  H  HO OOOOOOOO OHH  H   P   *                   %                                                          *",
    "*  H  H             H  H       *         L         %                                                T         *",
    "*  H  H          T  H  H       *    A        A     %HH*                                       T           T   *",
    "*  H  H             H  H       *                   %                                                T         *",
    "*  H%%HOOOOOOOOOOOOOH%%H       ***********                               L                    T           T   *",
    "*       L                      *                                                                    T         *",
    "*                           P  *                   %                                                          *",
    "********************************%%%%%%%%%%%%%%%%%%%%%%%%%%%%%HHHHHH%%%%%%%%HHHHHHHH%%%%%%HHHHHHH%%%%%%HHHHHHH**"
]

local
  fun chToWallType c =
      case c of
        #" " => 0
      | #"*" => 1
      | #"=" => 2
      | #"w" => 2
      | #"O" => 2
      | #"m" => 3
      | #"%" => 3
      | #"H" => 4
      | #"T" => 100
      | #"A" => 101
      | #"P" => 102
      | #"L" => 103
      | #"B" => 104
      | #"C" => 105
      | _ => raise Fail ("unknown character '" ^ Char.toString c ^ "'")
  fun line (s:string) : int list = CharVector.foldr (fn (c,a) => chToWallType c :: a) [] s                              
in val Map : int Array2.array = Array2.fromList (List.map line Smap)
end

structure Sprite = struct

datatype kind = Table | Armor | Plant | Lamp | Coin | Bread

fun toKind i =
    let open MapObj       
    in if not (isSprite i) then NONE
       else if i = T then SOME Table
       else if i = A then SOME Armor
       else if i = P then SOME Plant
       else if i = L then SOME Lamp
       else if i = C then SOME Coin
       else if i = B then SOME Bread
       else NONE
    end

fun blocking k =
    case k of
      Table => true
    | Armor => true
    | Plant => true
    | Lamp => false
    | Coin => false
    | Bread => false

fun img k =
    case k of
      Table => "tablechairs.png"
    | Armor => "armor.png"
    | Plant => "plantgreen.png"
    | Lamp => "lamp.png"
    | Coin => "score.png"
    | Bread => "time.png"

type t = {img: Js.elem, visible: bool ref, block: bool, pos: int * int, enabled: bool ref}

fun new typ pos =
    let val e = Js.createElement "img"
        val () = JsCore.setProperty (Js.Element.toForeignPtr e) JsCore.string "src" (img typ)
        val () = Js.setStyle e ("display", "none")
        val () = Js.setStyle e ("position", "absolute")
    in {img=e, block=blocking typ, visible=ref false, pos=pos, enabled=ref true}
    end

fun init items =
    let val screen = $"screen"
        val (rows,cols) = Array2.dimensions Map
        val spriteMap : t option Array2.array = Array2.array(rows,cols,NONE)
    in List.app (fn (k,x,y) => let val s = new k (x,y)
                               in Array2.update(spriteMap,y,x,SOME s);
                                  Js.appendChild screen (#img s)
                               end) items;
       spriteMap
    end    
fun region a = {base=a,row=0,col=0,nrows=NONE,ncols=NONE}

fun mapItems Map =
    Array2.foldi Array2.RowMajor (fn (y,x,i,acc) => 
                                     case toKind i of
                                       SOME k => (k,x,y)::acc
                                     | NONE => acc) [] (region Map)
end

val MaxRotSpeed = 3.0 * Math.pi / 360.0 (* how much does the player rotate each step/update (in radians) *) 

val player = {
   x = ref 16.0,                    (* current x, y position *)
   y = ref 10.0,
   rot = ref 0.0,                   (* the current angle of rotation *)
   speed = ref 0.0,                 (* the playing moving forward (speed = 1) or backwards (speed = -1) *)
   rotSpeed = ref 0.0,              (* the direction that the player is turning, between -1.0 for left and 1.0 for right *)
   moveSpeed = 0.10,                (* how far (in map units) does the player move each step/update *)
   rotAcc = ref 0.0
}

val mapWidth       = Array2.nCols Map
val mapWidthR      = real mapWidth
val mapHeight      = Array2.nRows Map
val mapHeightR     = real mapHeight
val miniMapScale   = 8
val miniMapScaleR  = real miniMapScale
val screenWidthR   = real screenWidth
val screenHeightR  = real screenHeight
val stripWidth     = 2
val stripWidthR    = real stripWidth
val fov            = 60.0 * Math.pi / 180.0
val numRays        = Real.ceil(screenWidthR / stripWidthR)
val fovHalf        = fov / 2.0
val viewDist       = screenWidthR / 2.0 / Math.tan(fovHalf)
val twoPI          = Math.pi * 2.0
val numTextures    = 4

fun installDocHandler s (f:int->unit) : unit =
    JsCore.exec1 {stmt="document." ^ s ^ " = function(e) { e = e || window.event; f(e.keyCode); };",
                  res=JsCore.unit,
                  arg1=("f",JsCore.==>(JsCore.int,JsCore.unit))} f

fun installOnkeydownHandler f = installDocHandler "onkeydown" f
fun installOnkeyupHandler f = installDocHandler "onkeyup" f

val rotAccDelta = 0.1

fun bindKeys() =
    let val () = installOnkeydownHandler 
                     (fn 38 => #speed player := 1.0   (* up, move player forward, ie. increase speed *)
	               | 40 => #speed player := ~1.0  (* down, move player backward, set negative speed *)
	               | 37 => let val rotAcc = #rotAcc player
                               in if !rotAcc > ~0.5 then rotAcc := !rotAcc - rotAccDelta  (* left, rotate player left *)
                                  else ()
                               end
	               | 39 => let val rotAcc = #rotAcc player
                               in if !rotAcc < 0.5 then rotAcc := !rotAcc + rotAccDelta  (* right, rotate player right *)
                                  else ()
                               end
                       | _ => ()
                     )
        val () = installOnkeyupHandler
                     (fn 38 => #speed player := 0.0     (* stop the player movement when up/down key is released *)
	               | 40 => #speed player := 0.0
	               | 37 => (#rotSpeed player := 0.0; #rotAcc player := 0.0)  (* stop the player movement when left/right key is released *)
	               | 39 => (#rotSpeed player := 0.0; #rotAcc player := 0.0)
                       | _ => ()
                     )
    in ()
    end

infix +=
fun a += (n:real) = a := !a + n

fun isWall k = 
    k > 0 andalso not(MapObj.isSprite k)

fun isBlocking spriteMap (x,y) =
    (y < 0.0 orelse y >= mapHeightR orelse x < 0.0 orelse x >= mapWidthR) orelse
    let val Y = Real.floor y
        val X = Real.floor x
        val wt = Array2.sub(Map, Y, X)
    in isWall wt orelse
       case Array2.sub(spriteMap,Y,X) of
         SOME (s:Sprite.t) => #block s
       | NONE => false
    end

fun checkCollision spriteMap (fromX, fromY, toX, toY, radius) =
    if toY < 0.0 orelse toY >= mapHeightR orelse toX < 0.0 orelse toX >= mapWidthR then
      (fromX,fromY)
    else
      let val blockX = real (Real.floor toX)
	  val blockY = real (Real.floor toY)
          val isBlock = isBlocking spriteMap
      in if isBlock(blockX,blockY) then (fromX,fromY)
	 else
           let 
	     val blockTop = isBlock(blockX,blockY-1.0)
	     val blockBottom = isBlock(blockX,blockY+1.0)
	     val blockLeft = isBlock(blockX-1.0,blockY)
	     val blockRight = isBlock(blockX+1.0,blockY)
             val toY =
                 if blockTop andalso toY - blockY < radius then blockY + radius
                 else if blockBottom andalso blockY+1.0 - toY < radius then blockY + 1.0 - radius
                 else toY
             val toX =
                 if blockLeft andalso toX - blockX < radius then blockX + radius
                 else if blockRight andalso blockX+1.0 - toX < radius then blockX + 1.0 - radius
                 else toX
           in (toX,toY)
           end
      end

infix ==
fun a == (b:real) = a >= b andalso b >= a

fun minimum (a:real) b = if a > 0.0 then 
                           if a > b then b else a
                         else
                           if a < ~b then ~b else a

fun move spriteMap gameCycleDelay timeDelta =    
    let val mul = real (IntInf.toInt timeDelta) / real (IntInf.toInt gameCycleDelay)
        val moveStep = mul * !(#speed player) * #moveSpeed player (* player will move this far along the current direction vector *)
        val () = #rotSpeed player := minimum (!(#rotSpeed player) + !(#rotAcc player)) 2.0
        val drot = mul * !(#rotSpeed player) * MaxRotSpeed        (* add rotation if player is rotating (!player#dir <> 0) *)
	val () = #rot player += drot
        val dx = Math.cos(!(#rot player)) * moveStep
        val dy = Math.sin(!(#rot player)) * moveStep
(*
        val () = if moveStep > 0.0 then
                   log ("dx=" ^ Real.toString dx ^ "; dy=" ^ Real.toString dy ^ "; drot=" ^ Real.toString drot ^ "; moveStep=" ^ Real.toString moveStep) 
                 else ()
*)
        val fromX = !(#x player)
        val fromY = !(#y player)
        val (newX,newY) = checkCollision spriteMap (fromX,fromY,fromX+dx,fromY+dy,0.35)
    in #x player := newX;
       #y player := newY
    end

structure C = Canvas

fun getElemProperty e t s =
    JsCore.getProperty (Js.Element.toForeignPtr e) t s

fun updateMiniMap() =
    let val miniMap = $ "minimap"
	val miniMapObjects = $ "minimapobjects"
        val height = getElemProperty miniMap JsCore.int "height"
        val width = getElemProperty miniMap JsCore.int "width"
	val c = C.getContext miniMapObjects "2d"
	val () = C.clearRect c 0 0 width height

	val () = (* draw a dot at the current player position *)
            C.fillRect c (Real.round(!(#x player) * miniMapScaleR - 2.0))
                       (Real.round(!(#y player) * miniMapScaleR - 2.0))
                       4 4
	val () = C.beginPath c
        val () = C.moveTo c (Real.round(!(#x player) * miniMapScaleR))
                          (Real.round(!(#y player) * miniMapScaleR))
	val () = C.lineTo c
                          (Real.round((!(#x player) + Math.cos(!(#rot player))) * miniMapScaleR))
                          (Real.round((!(#y player) + Math.sin(!(#rot player))) * miniMapScaleR))
    in C.closePath c;
       C.stroke c
    end

fun setElemPropertyInt e p i =
    JsCore.setProperty (Js.Element.toForeignPtr e) JsCore.int p i

fun drawRay(rayX, rayY) =
    let val miniMapObjects = $ "minimapobjects"
	val c = C.getContext miniMapObjects "2d"
    in
      C.strokeStyle c "rgba(0,100,0,0.3)";
      C.lineWidth c "0.5";
      C.beginPath c;
      C.moveTo c (Real.round(!(#x player) * miniMapScaleR)) (Real.round(!(#y player) * miniMapScaleR));
      C.lineTo c (Real.round(rayX * miniMapScaleR)) (Real.round (rayY * miniMapScaleR));
      C.closePath c;
      C.stroke c
    end

fun drawMiniMap() =
    (* draw the topdown view minimap *)
    let	val miniMap = $ "minimap"               (* the actual map *)
	val miniMapCtr = $ "minimapcontainer"	(* the container div element *)
	val miniMapObjects = $ "minimapobjects" (* the canvas used for drawing the objects on the map (player character, etc) *)

        val w = mapWidth * miniMapScale
        val h = mapHeight * miniMapScale
	val () = setElemPropertyInt miniMap "width" w   (* resize the internal canvas dimensions *)
	val () = setElemPropertyInt miniMap "height" h (* of both the map canvas and the object canvas *)
	val () = setElemPropertyInt miniMapObjects "width" w
	val () = setElemPropertyInt miniMapObjects "height" h
	val () = setElemPropertyInt miniMapCtr "width" w
	val () = setElemPropertyInt miniMapCtr "height" h

	val w_s = ppInt w ^ "px" (* minimap CSS dimensions *)
	val h_s = ppInt h ^ "px"
	val () = Js.setStyle miniMap ("width",w_s)
        val () = Js.setStyle miniMapObjects ("width",w_s)
        val () = Js.setStyle miniMapCtr ("width", w_s)
	val () = Js.setStyle miniMap ("height",h_s)
        val () = Js.setStyle miniMapObjects ("height",h_s)
        val () = Js.setStyle miniMapCtr ("height", h_s)

	val c = C.getContext miniMap "2d"

	(* loop through all blocks on the map *)
        val entireMap = {base=Map,row=0,col=0,nrows=NONE,ncols=NONE}

        fun each (y,x,k) =
            if isWall k then (* if there is a wall block at this (x,y) ...*)
              (* ... then draw a block on the minimap *)
              (C.fillStyle c "rgb(200,200,200)";
               C.fillRect c (x * miniMapScale) (y * miniMapScale) miniMapScale miniMapScale)
            else ()

        val () = Array2.appi Array2.RowMajor each entireMap
    in
	updateMiniMap()
    end

fun RealMod(r:real,m) =
    if r < m then r
    else RealMod(r - m, m)

structure Strip = struct
type data = {width:int ref,height:int ref,left:int ref,top:int ref,clip:string ref,zIndex:int ref}
fun emptyData() = {width=ref ~1,height=ref ~1,left=ref ~1,top=ref ~1,clip=ref "",zIndex=ref ~1}
fun setProp pp (prop:string) (sel:data->int ref) (strip,data:data) (n:int) : unit =
    let val r = sel data
    in if !r = n then ()
       else (Js.setStyle strip (prop, pp n);
             r := n)
    end
fun ppPx n = ppInt n ^ "px"
val setWidth = setProp ppPx "width" #width
val setHeight = setProp ppPx "height" #height
val setTop = setProp ppPx "top" #top
val setLeft = setProp ppPx "left" #left
val setZindex = setProp ppInt "zIndex" #zIndex

fun setClip (strip,data:data) (c:string) : unit =
    let val r = #clip data
    in if !r = c then ()
       else (Js.setStyle strip ("clip", c);
             r := c)
    end
end

fun castSingleRay screenStrips (spriteMap: Sprite.t option Array2.array) (rayAngle, stripIdx, visibleSprites) =
    let
      (* make sure the angle is between 0 and 360 degrees *)
      val rayAngle = RealMod (rayAngle, twoPI)
      val rayAngle = if rayAngle < 0.0 then rayAngle + twoPI else rayAngle
                                                                  
      (* moving right/left? up/down? Determined by which quadrant the angle is in. *)
      val right = rayAngle > twoPI * 0.75 orelse rayAngle < twoPI * 0.25
      val up = rayAngle < 0.0 orelse rayAngle > Math.pi
                                     
      (* only do these once *)
      val angleSin = Math.sin rayAngle
      val angleCos = Math.cos rayAngle
                     
      val textureX = 0 (* the x-coord on the texture of the block, ie. what part of the texture are we going to render *)
                     
      (* first check against the vertical map/wall lines
       * we do this by moving to the right or left edge of the block we're standing in
       * and then moving in 1 map unit steps horizontally. The amount we have to move vertically
       * is determined by the slope of the ray, which is simply defined as sin(angle) / cos(angle).
       *)
                     
      val slope = angleSin / angleCos	(* the slope of the straight line made by the ray *)
      val dXVer = if right then 1.0 else ~1.0 (* we move either 1 map unit to the left or right *)
      val dYVer = dXVer * slope (* how much to move up or down *)
                  
      val x = if right then Real.ceil (!(#x player)) else Real.floor(!(#x player)) (* starting horizontal position, at one of the edges of the current map block *)
      val x = real x
      val y = !(#y player) + (x - (!(#x player))) * slope (* starting vertical position. We add the small horizontal step we just made, multiplied by the slope. *)
              
      fun loop (x, y) visibleSprites =
          if x < 0.0 orelse x >= mapWidthR orelse y < 0.0 orelse y >= mapHeightR then
            ({xHit = 0.0, yHit = 0.0, dist = 0.0, textureX = 0.0, xWallHit = 0, yWallHit = 0, wallType = 0},visibleSprites)
          else 
            let
	      val wallX = Real.floor (if right then x else x - 1.0)
	      val wallY = Real.floor y		
              val wt = Array2.sub(Map, wallY, wallX)
            in 
	      if isWall wt then (* is this point inside a wall block? *)
                let val distX = x - !(#x player)
		    val distY = y - !(#y player)
	            val dist = distX*distX + distY*distY (* the distance from the player to this point, squared *)
                               
		    val textureX = y - real (Real.floor y) (* where exactly are we on the wall? textureX is the x coordinate 
                                                            * on the texture that we'll use later when texturing the wall. *)
                    val textureX = if not right then 1.0 - textureX else textureX (* if we're looking to the left side of the 
                                                                                   * map, the texture should be reversed *)
                in (* save the coordinates of the hit. We only really use these to draw the rays on minimap *)
                  ({xHit=x, yHit=y, wallType=wt, dist=dist, xWallHit=wallX, yWallHit=wallY, textureX=textureX},visibleSprites)
                end
              else
                loop (x+dXVer,y+dYVer)
                     (case Array2.sub(spriteMap,wallY,wallX) of
                        NONE => visibleSprites
                      | SOME sprite => if !(#visible sprite) then visibleSprites
                                       else (#visible sprite := true; sprite::visibleSprites))
            end                 
      val ({xHit, yHit, dist, textureX, xWallHit, yWallHit, wallType},visibleSprites) = loop (x,y) visibleSprites
                                                                                   
      (* now check against horizontal lines. It's basically the same, just "turned around".
       * the only difference here is that once we hit a map block, 
       * we check if there we also found one in the earlier, vertical run. We'll know that if dist != 0.
       * If so, we only register this hit if this distance is smaller. *)
                                                                                   
      val slope = angleCos / angleSin
      val dYHor = if up then ~1.0 else 1.0
      val dXHor = dYHor * slope
      val y = real(if up then Real.floor(!(#y player)) else Real.ceil(!(#y player)))
      val x = !(#x player) + (y - !(#y player)) * slope
              
      fun loop (x,y) visibleSprites =
	  if (x < 0.0 orelse x >= mapWidthR orelse y < 0.0 orelse y >= mapHeightR orelse slope > 10000.0) then
            ({dist=dist,xHit=xHit,yHit=yHit,xWallHit=xWallHit,yWallHit=yWallHit,wallType=wallType,textureX=textureX},visibleSprites)
          else
            let val wallY = Real.floor (if up then y - 1.0 else y)
	        val wallX = Real.floor x
                val wt = Array2.sub(Map,wallY,wallX)
                    handle ? => 
                           (log ("wallX = " ^ ppInt wallX ^ "; wallY = " ^ ppInt wallY); raise ?)
            in if isWall wt then
                 let val distX = x - !(#x player)
		     val distY = y - !(#y player)
                     val blockDist = distX*distX + distY*distY
                 in
		   if not (dist > 0.0) orelse blockDist < dist then
                     let val textureX = x - real(Real.floor x)
                         val textureX = if up then 1.0 - textureX else textureX
                     in ({dist=blockDist, xHit=x, yHit=y, xWallHit=wallX, yWallHit=wallY, wallType=wt, textureX=textureX},visibleSprites)
                     end
                   else ({dist=dist,xHit=xHit,yHit=yHit,xWallHit=xWallHit,yWallHit=yWallHit,wallType=wallType,textureX=textureX},visibleSprites)
                 end
               else loop (x+dXHor,y+dYHor)
                         (case Array2.sub(spriteMap,wallY,wallX) of
                            NONE => visibleSprites
                          | SOME sprite => if !(#visible sprite) then visibleSprites
                                           else (#visible sprite := true; sprite::visibleSprites))
            end
      val ({dist,xHit,yHit,xWallHit,yWallHit,wallType,textureX},visibleSprites) = loop(x,y)visibleSprites
    in
      if dist <= 0.0 then visibleSprites
      else
	let (* val () = drawRay(xHit, yHit) *)
	    val	dist = Math.sqrt dist
	    (* use perpendicular distance to adjust for fish eye
	     * distorted_dist = correct_dist / cos(relative_angle_of_ray) *)
	    val dist = dist * Math.cos(!(#rot player) - rayAngle)
                       
	    (* now calc the position, height and width of the wall strip *)
                       
	    (* "Real" wall height in the game world is 1 unit, the distance from the player to the screen is viewDist,
	     * thus the height on the screen is equal to wall_height_real * viewDist / dist *)
                       
	    val height = Real.round(viewDist / dist)
                         
	    (* width is the same, but we have to stretch the texture to a factor of stripWidth to make it fill the strip correctly *)
	    val width = height * stripWidth
            val widthR = real width
            val heightR = real height
	    (* top placement is easy since everything is centered on the x-axis, so we simply move
	     * it half way down the screen and then half the wall height back up. *)
	    val top = Real.round((screenHeightR - heightR) / 2.0)
	    val texX = Real.round(textureX * widthR)
            val texX = if texX > width-stripWidth then width - stripWidth else texX
                      
            val stripdata = Vector.sub(screenStrips, stripIdx)
            val () = Strip.setHeight stripdata height
            val () = Strip.setTop stripdata top
                     
            val imgTop = Real.floor(heightR * real (wallType-1))
            val styleHeight = Real.floor(heightR * real numTextures)
            val () = Strip.setHeight stripdata styleHeight

            val styleWidth = Real.floor(widthR * 2.0)
            val () = Strip.setWidth stripdata styleWidth

            val styleTop = top - imgTop
            val () = Strip.setTop stripdata styleTop

            val styleLeft = stripIdx*stripWidth - texX
            val () = Strip.setLeft stripdata styleLeft

            val styleClip =
                "rect(" ^ ppInt imgTop ^ "," ^ ppInt(texX + stripWidth) ^
                "," ^ ppInt(imgTop + height) ^ "," ^ ppInt texX ^ ")"
            val () = Strip.setClip stripdata styleClip
                     
	    val dwx = real xWallHit - !(#x player)
	    val dwy = real yWallHit - !(#y player)
	    val wallDist = dwx*dwx + dwy*dwy
	    val () = Strip.setZindex stripdata (~(Real.floor(wallDist*1000.0)))
        in visibleSprites
        end
    end

fun castRays screenStrips spriteMap =
    (* we accumulate the collected visible sprites *)
    let fun loop i acc =
            if i >= numRays then acc
            else
              let
                (* where on the screen does ray go through? *)
		val rayScreenPos = (~ (real numRays) / 2.0 + real i) * stripWidthR;

		(* the distance from the viewer to the point on the screen, simply Pythagoras *)
		val rayViewDist = Math.sqrt(rayScreenPos*rayScreenPos + viewDist*viewDist)

		(* the angle of the ray, relative to the viewing direction.
		 * right triangle: a = sin(A) * c *)
		val rayAngle = Math.asin(rayScreenPos / rayViewDist)
                val acc =
		    castSingleRay screenStrips spriteMap
                                  (!(#rot player) + rayAngle, (* add the players viewing direction to get the angle in world space *)
			           i, acc)
              in loop(i + 1) acc
              end
    in loop 0 []
    end

local
  val zero = Time.now()
in
fun now() =
    let val n = Time.now()
    in Time.toMilliseconds(Time.-(n,zero))
    end
end

fun gameCycle spriteMap =
    let val gameCycleDelay = 1000 div 30 (* aim for 30 FPS *)
        fun cycle lastGameCycleTime () =
            let val n = now()
                val timeDelta = n - lastGameCycleTime
                val _ = move spriteMap gameCycleDelay timeDelta
                val cycleDelay = if timeDelta > gameCycleDelay then
                                   IntInf.max(1, gameCycleDelay - (timeDelta - gameCycleDelay))
                                 else gameCycleDelay
            in Js.setTimeout (IntInf.toInt cycleDelay) (cycle n);
               ()
            end
    in cycle 0 ()
    end

fun textUpdater id pp =
    let val old = ref ""
    in fn v =>
          let val s = pp v
          in if !old = s then ()
             else
               let val e = $ id
               in Js.innerHTML e s; old := s
               end
          end
    end

val updateOverlay =
    textUpdater "overlay" (fn x => "FPS: " ^ Real.fmt (StringCvt.FIX (SOME 0)) x)

val updateScore =
    textUpdater "score" (fn x => "SCORE: " ^ Int.toString x)

fun renderSprites oldSprites sprites =
    (List.app (fn s:Sprite.t =>
                  let val (x,y) = #pos s
                      val dx = real x + 0.5 - !(#x player)
                      val dy = real y + 0.5 - !(#y player)
                      val dist = Math.sqrt (dx*dx + dy*dy)   (* distance to sprite *)
                      val spriteAngle = Math.atan2(dy,dx) - !(#rot player) (* sprite angle relative to viewing angle *)
                      val size = viewDist / (Math.cos spriteAngle * dist)
                  in if size <= 0.0 then ()
                     else 
                       let val img = #img s
                           val () = Js.setStyle img ("display","block")
                           val screen_x = Math.tan spriteAngle * viewDist (* x-position on screen *)
                           val left = Real.round (screenWidthR / 2.0 + screen_x - size / 2.0)
                           val top = Real.round ((screenHeightR-size)/2.0)
                           val () = Js.setStyle img ("left", ppInt left ^ "px") 
                           val () = Js.setStyle img ("top", ppInt top ^ "px")
                           val dbx = real x - !(#x player)
                           val dby = real y - !(#y player)
                           val sz = ppInt (Real.round size) ^ "px"
                           val () = Js.setStyle img ("width", sz) 
                           val () = Js.setStyle img ("height", sz) 
                           val blockDist = dbx*dbx + dby*dby
                           val zIndex = ~ (Real.floor (blockDist*1000.0))
                           val () = Js.setStyle img ("zIndex", ppInt zIndex)
                       in ()
                              (* log ("rendering sprite: (" ^ ppInt x ^ "," ^ ppInt y ^ "); left=" ^ ppInt left ^ "; top=" ^ ppInt top ^ "; size=" ^ Real.toString size ^ "; sz=" ^ sz) *)
                       end
                  end) sprites;
     (* hide the sprites that are no longer visible *)
     List.app (fn s:Sprite.t => if not (!(#visible s)) then
                                  (Js.setStyle (#img s) ("display","none") (* ;
                                                                            log ("hiding sprite at (" ^ ppInt(#1(#pos s)) ^ "," ^ ppInt(#2(#pos s)) ^ ")") *)
                                  )
                                else () (* part of visible sprites *)
              ) oldSprites)
    
                               
fun clearSprites sprites =
    List.app (fn s:Sprite.t => #visible s := false) sprites

val score = ref 0

fun renderCycle screenStrips spriteMap =
    let val renderCycleDelay = 1000 div 30 (* aim for 30 FPS *)
        fun cycle (lastRenderCycleTime,oldSprites) () =
            let val () = updateMiniMap()
                val () = clearSprites oldSprites
                val sprites = castRays screenStrips spriteMap
                val () = renderSprites oldSprites sprites
                val n = now()
                val timeDelta = n - lastRenderCycleTime
                val cycleDelay = if timeDelta > renderCycleDelay then 
		                   IntInf.max(1, renderCycleDelay - (timeDelta - renderCycleDelay))
                                 else renderCycleDelay
                val cycleDelay = IntInf.toInt cycleDelay
	        val _ =	Js.setTimeout cycleDelay (cycle (n,sprites))
                val fps = 1000.0 / real (IntInf.toInt timeDelta)
            in updateOverlay fps;
               updateScore (!score)
            end
    in cycle (0,[]) ()
    end

fun initScreen() =
    let val screen = $ "screen"
        fun loop i =
            if i >= screenWidth then []
            else
              let val strip = Js.createElement "img"
                  val () = Js.setStyle strip ("position", "absolute")
                  val () = JsCore.setProperty (Js.Element.toForeignPtr strip) JsCore.string "src" "walls.png"
                  val stripdata = (strip,Strip.emptyData())
                  val () = Strip.setHeight stripdata 0
                  val () = Strip.setLeft stripdata 0
                  val () = Strip.setTop stripdata 0
                  val () = Js.appendChild ($"screen") strip
              in stripdata :: loop (i + stripWidth)
              end
    in loop 0
    end

fun init() =
   let val () = bindKeys()
       val screenStrips = Vector.fromList (initScreen())
       val spriteMap = Sprite.init (Sprite.mapItems Map)
   in drawMiniMap();
      gameCycle spriteMap;
      renderCycle screenStrips spriteMap
   end

in
val _ = Js.setTimeout 1 init
end
