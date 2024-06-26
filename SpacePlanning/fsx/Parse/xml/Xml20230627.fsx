(* let xmls = """
<residence id="0">                        
  <foyer id="1" size="5">            
    <study id="1.1" size="10"></study>                                    
  </foyer>                           
  <living id="2" size="20"></living> 
  <dining id="3" size="20">          
    <bed-1 id="3.1" size="15">       
      <bath-1 id="3.1.1" size="5"></bath-1>                               
    </bed-1>                         
    <bed-2 id="3.2" size="15">       
      <dress-2 id="3.2.1" size="5">  
        <bath-2 id="3.2.1.1" size="5"></bath-2>                           
      </dress-2>                     
    </bed-2>                         
    <bed-3 id="3.3" size="15">       
      <dress-3 id="3.3.1" size="5"></dress-3>                             
      <bath-3 id="3.3.2" size="5"></bath-3>                               
    </bed-3>                         
    <kitchen id="3.4" size="15">     
      <utility id="3.4.1" size="5"></utility>                             
    </kitchen>                       
  </dining>                          
  <staircase id="4" size="15"></staircase>                                
</residence>
"""
 *)

open System.Xml.Linq
let xx = XElement.Load("""C:\Users\vykru\Github\Hexel\SpacePlanning\fsx\Parse\xml\space1.xml""")

//let xe= XmlReader.Create ("""C:\Users\vykru\Github\Hexel\SpacePlanning\fsx\Parse\xml\space1.xml""")
let xa = xx.Elements()
let xb = xa |> Seq.map (fun x -> x.Name.ToString()) |> String.concat ","
let xc = (xb.Split ",") |> Array.map (fun x -> xx.Element(x))


