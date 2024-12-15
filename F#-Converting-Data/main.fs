open System.Xml

let xmlString = "<root><person><name>John</name><age>30</age></person></root>"

let xmlDoc = new XmlDocument()
xmlDoc.LoadXml(xmlString)

let rec xmlNodeToJson (node: XmlNode) =
    match node.NodeType with
    | XmlNodeType.Element ->
        let children = [| for child in node.ChildNodes -> xmlNodeToJson child |]
        sprintf "{\"%s\":%s}" node.Name (String.concat "," children)
    | XmlNodeType.Text ->
        sprintf "\"%s\"" (node.Value.Trim())
    | _ -> ""

let json = xmlNodeToJson xmlDoc.DocumentElement

printfn "%s" json