let xmls = """
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

// Test
open System.Xml.Linq
let xd = XDocument.Load("""C:\Users\vykru\Github\Hexel\SpacePlanning\fsx\Parse\xml\space1.xml""")

let rec processElement (element: XElement) (path: string) =
    let size = element.Attribute("size").Value
    let newPath = sprintf "%s.%s" path element.Name.LocalName
    let output = sprintf "(%s/%s/%s)" newPath size element.Name.LocalName
    
    let childElements = element.Elements()
    if Seq.isEmpty childElements then
        output
    else
        let childrenOutput = childElements |> Seq.map (fun e -> processElement e newPath)
        sprintf "%s,%s" output (String.concat"," childrenOutput)

let output = xd.Root.Elements() |> Seq.map (fun e -> processElement e "1") |> String.concat(",")

printfn "%s" output
