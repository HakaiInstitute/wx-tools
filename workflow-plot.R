library(DiagrammeR)

graph <- grViz(
  "digraph process_flow {
    graph [layout = dot, rankdir = TB]
    node [shape = rectangle, style = filled, fillcolor = lightblue, fontname = Helvetica]
    
    PreSeason [label="Pre Season", shape=oval, fillcolor=lightgray]
    SiteMaps [label="Site Maps"]
    Travel [label="Travel and Logistics"]
    RiskAssessment1 [label="Risk Assessment"]
    Orientation [label="Orientation(s), SWPs & SOPs"]
    EmergencyPlan [label="Emergency Response Plan"]
    Certifications [label="Certifications & Education"]
    
    DayOf [label="Day Of", shape=oval, fillcolor=lightgray]
    Tailgate [label="Tailgate/Toolbox Meeting"]
    RiskAssessment2 [label="Risk Assessment"]
    FileTripPlan [label="File Trip Plan"]
    TripTasks [label="Trip Tasks"]
    
    PostReview [label="Post Review", shape=oval, fillcolor=lightgray]
    Review [label="Review"]
    
    # Edges for Pre-Season
    PreSeason -> SiteMaps
    PreSeason -> Travel
    PreSeason -> RiskAssessment1
    PreSeason -> Orientation
    PreSeason -> EmergencyPlan
    PreSeason -> Certifications
    
    # Edges for Day Of
    DayOf -> Tailgate
    DayOf -> RiskAssessment2
    DayOf -> FileTripPlan
    DayOf -> TripTasks
    
    # Post Review
    PostReview -> Review
    
    # Flow connections
    Certifications -> DayOf
    TripTasks -> PostReview
  }"
)

graph
