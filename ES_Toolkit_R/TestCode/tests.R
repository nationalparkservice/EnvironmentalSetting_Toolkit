# Must use correct parameter name: uid unless only using sid values.
# elems can be a character vector - need to determine how to produce: 
# elems: [{
#   name: "avgt",
#   add: "f,s"
# }, {
#   name: "mint",
#   add: "f,s"
# }, {
#   name: "maxt",
#   add: "f,s"
# }]
# Service accepts a vector c(), can this be expanded?  Note POST encode = "form" does not work with c() as input
# Look as POST form encoding examples or complex vectors
POST("http://data.rcc-acis.org/StnData", body = list(uid = "25056", sdate = "20150801", edate = "20150831", elems = c('obst', 'mint', 'maxt', 'pcpn', 'avgt')), encode = "json", verbose())

json <- elems: [{
     name: "avgt",
     add: "f,s"
   }, {
     name: "mint",
     add: "f,s"
   }, {
     name: "maxt",
     add: "f,s"
   }]

# Pseudo code for creating elems item:
e1 <- list(name=c("avgt"),add=c("f,s"))
e2 <- list(name=c("mint"),add=c("f,s"))
e3 <- list(name=c("maxt"),add=c("f,s"))
el <- list(e1, e2, e3)
elems <- toJSON(el, auto_unbox = TRUE)

POST("http://data.rcc-acis.org/StnData", body = list(uid = "25056", sdate = "20150801", edate = "20150831", elems = elems), encode = "json", verbose())

bList <- list(sid = "USC00438556", sdate = "20150801", edate = "20150831", elems = elems)

bJSON <- gsub("\\\\","T", toJSON(body, auto_unbox = TRUE))
f = gsub("\"\\[","\\[", bJSON)
g = gsub("\\]\"","\\]", f)
h = gsub("T","", g)
i = gsub("\"\"\\{","\\{",h)
body <- gsub("\\}\"\"","\\}",i)

POST("http://data.rcc-acis.org/StnData", accept_json(), add_headers("Content-Type" = "application/json"), body = body, verbose())

POST("http://data.rcc-acis.org/StnData", body = body, encode = "json", verbose())
POST("http://data.rcc-acis.org/StnData", body = body, verbose())

e1 <- list(name=c("avgt"))
e2 <- list(name=c("mint"))
e3 <- list(name=c("maxt"))
el <- list(e1, e2, e3)
names(dimnames(el[1])) <- gsub('"','', el[1]))
elems <- toJSON(el, auto_unbox = TRUE)


### THIS CODE WORKS ####

e1 <- list(name=c("avgt"),add=c("f,s"))
e2 <- list(name=c("mint"),add=c("f,s"))
e3 <- list(name=c("maxt"),add=c("f,s"))
el <- list(e1, e2, e3)
elems <- toJSON(el, auto_unbox = TRUE)
bList <- list(sid = "USC00438556", sdate = "20150801", edate = "20150831", elems = elems)
bJSON <- gsub("\\\\","T", toJSON(body, auto_unbox = TRUE))
f = gsub("\"\\[","\\[", bJSON)
g = gsub("\\]\"","\\]", f)
h = gsub("T","", g)
i = gsub("\"\"\\{","\\{",h)
body <- gsub("\\}\"\"","\\}",i)

POST("http://data.rcc-acis.org/StnData", accept_json(), add_headers("Content-Type" = "application/json"), body = body, verbose())
