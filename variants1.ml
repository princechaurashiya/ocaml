type  day = Sun |Mon |Tue |Wed |Thu |Fri |Sat
 let d = Tue 
 
 let day_of_int d =
  match d with 
  |Sun ->"hollyday"  
  |Mon ->"Not hollyday"  
  |Tue ->"Not hollyday"  
  |Wed ->"Not hollyday"  
  |Thu ->"Not hollyday"  
  |Fri ->"Not hollyday"  
  |Sat ->"hollyday"  
   
  (* Maulana Abul Kalam Azad University of Technology 
     Formerly known as wbut : west Bengal university of technology *)