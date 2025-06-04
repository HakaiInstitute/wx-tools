library(DiagrammeR)

grViz("
  digraph workflow {
    graph [rankdir = TB, style = solid]
    
    node [shape = rectangle, style = filled, fillcolor = lightblue, labeljust = 'center', labelloc = 'c']
    
    A [label = 'Download Data from Tidbit Sensors']
    B [label = 'Push to Repository (Raw Data)']
    C [label = 'Store in Database (Raw Data Table)']
    D [label = 'Pull Data into R for QC']
    
    subgraph cluster_QC {
      label = 'Quality Control (QC)'
      labeljust = 'left'
      labelloc = 't'
      style = dashed
      color = grey
      E1 [label = 'Automated QC: Flag Outliers, Gaps', fillcolor = orange]
      E2 [label = 'Manual QC: Compare with Field Notes & Reference Data', fillcolor = palegreen]
    }
    
    F [label = 'Store Cleaned Data in Database (QC’d Table)']
    G [label = 'Push Cleaned Data Back to Repository (Versioned QC’d Data)']
    H [label = 'Collaborators Access Cleaned Data']

    A -> B -> C -> D
    D -> E1
    D -> E2
    E1 -> F
    E2 -> F
    F -> G -> H
    
    
  }
")


library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

graph <- grViz("
  digraph process_flow {
    graph [layout = dot, rankdir = TB]
    node [shape = rectangle, style = filled, fontname = Helvetica]
    
    PreSeason [label=\"Pre-Season\", shape=oval, fillcolor=\"#D3D3D3\"]
    SiteMaps [label=\"Site Maps\", fillcolor=\"#56B4E9\"]
    Travel [label=\"Travel and Logistics\", fillcolor=\"#009E73\"]
    RiskAssessment1 [label=\"Risk Assessment\", shape=rectangle, style=dashed, fillcolor=\"#E69F00\"]
    Orientation [label=\"Orientation(s), SWPs & SOPs\", fillcolor=\"#F0E442\"]
    EmergencyPlan [label=\"Emergency Response Plan\", fillcolor=\"#0072B2\"]
    Certifications [label=\"Certifications & Education\", fillcolor=\"#CC79A7\"]
    
    # Workflow connections
    PreSeason -> SiteMaps
    SiteMaps -> Travel
    Travel -> RiskAssessment1
    RiskAssessment1 -> Orientation
    Orientation -> EmergencyPlan
    EmergencyPlan -> Certifications
  }"
)


#################################################################################################################################################

graph1 <- grViz("
  digraph process_flow {
    graph [layout = dot, rankdir = TB]
    node [shape = rectangle, style = filled, fontname = Helvetica]
    
    PreSeason [label=\"Field Day\", shape=oval, fillcolor=\"#D3D3D3\"]
    SiteMaps [label=\"Tailgate/Toolbox meeting\", fillcolor=\"#56B4E9\"]
    Travel [label=\"Travel and Logistics\", fillcolor=\"#009E73\"]
    RiskAssessment1 [label=\"Risk Assessment\", shape=rectangle, style=dashed, fillcolor=\"#E69F00\"]
    Orientation [label=\"File Trip Plan\", fillcolor=\"#F0E442\"]
    EmergencyPlan [label=\"Trip Tasks\", fillcolor=\"#0072B2\"]get_
    
    
    # Workflow connections
    PreSeason -> SiteMaps
    SiteMaps -> Travel
    Travel -> RiskAssessment1
    RiskAssessment1 -> Orientation
    Orientation -> EmergencyPlan
    
  }"
)
graph1

