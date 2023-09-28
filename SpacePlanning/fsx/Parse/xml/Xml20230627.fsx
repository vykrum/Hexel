let xmls = """
<residence>
    <foyer value="5">
        <study value="10"></study>
    </foyer>
    <living value="20"></living>
    <dining value="20">
        <bed-1 value="15">
            <bath-1 value="5"></bath-1>
        </bed-1>
        <bed-2 value="15">
            <dress-2 value="5">
                <bath-2 value="5"></bath-2>
            </dress-2>
        </bed-2>
        <bed-3 value="15">
            <dress-3 value="5"></dress-3>
            <bath-3 value="5"></bath-3>
        </bed-3>
        <kitchen value="15">
            <utility value="5"></utility>
        </kitchen>
    </dining>
    <staircase value="20"></staircase>
</residence>
"""
open System.Xml.Linq
let xd = XDocument.Load("""C:\Users\vykru\Github\Hexel\SpacePlanning\fsx\Parse\xml\space.xml""")
let books = xd.Elements() |> Seq.iter ( fun(e) -> do printfn "%s" e.Value )

