fun returnPage title body =
  (Ns.Quot.return `<html>
	             <head><title>^title</title></head>
		     <body bgcolor=white><h2>^title</h2>
		       ^(Ns.Quot.flatten body)
                     <hr> <i>Served by SMLserver</i>
		     </body>
		   </html>`;
   Ns.exit())

fun error s = returnPage ("Error: " ^ s)
  `An error occured while generating a recipe for 
   you; use your browser's back-button to back-up 
   and enter a number in the form.`

val persons = 
  case Ns.Conn.formvar "persons"
    of SOME s => (case Int.fromString s
		    of SOME n => real n
		     | NONE => error "You must type a number")
     | NONE => error "Query data missing"

fun pr_num s r = 
  if Real.== (r,1.0) then "one " ^ s
  else 
    if Real.==(real(round r),r) then 
      Int.toString (round r) ^ " " ^ s ^ "s"
    else Real.toString r ^ " " ^ s ^ "s"

val _ = returnPage "Apple Pie Recipe"
  `To make an Apple pie for ^(pr_num "person" persons), you 
   need the following ingredients:
   <ul> 
    <li> ^(pr_num "cup" (persons / 16.0)) butter
    <li> ^(pr_num "cup" (persons / 4.0)) sugar
    <li> ^(pr_num "egg" (persons / 4.0))
    <li> ^(pr_num "teaspoon" (persons / 16.0)) salt
    <li> ^(pr_num "teaspoon" (persons / 4.0)) cinnamon
    <li> ^(pr_num "teaspoon" (persons / 4.0)) baking soda
    <li> ^(pr_num "cup" (persons / 4.0)) flour
    <li> ^(pr_num "cup" (2.5 * persons / 4.0)) diced apples
    <li> ^(pr_num "teaspoon" (persons / 4.0)) vanilla
    <li> ^(pr_num "tablespoon" (persons / 2.0)) hot water
   </ul>

   Combine ingredients in order given. Bake in greased 9-inch 
   pie pans for 45 minutes at 350F. Serve warm with whipped 
   cream or ice cream. <p>

   Make <a href=recipe.html>another recipe</a>.`
  