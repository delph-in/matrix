function draw_hierarchy(type) {
  // type will equal 'noun' or 'verb' 
  var form = document.getElementsByTagName('form')[0];
  var ins = document.getElementsByTagName('input');
  var its = document.getElementsByClassName('iterator');
  var lts = new Array();
  var feats = new Object(); //keep a hash of features defined on types
  var inherited_feats = new Object();

  // figure out how many types are in the hierachy
  // and fill up the lts list with them
  // 
  // do this by looking through the iterators and 
  // finding applicable ones with regexes
  //
  var pat = RegExp(type+'[0-9]+$');
  for (var i = 0; i<its.length; i++){
    if (pat.test(its[i].id)) {
      lts[lts.length]=its[i].id;  
      // also, get this lex-type's features, if any, and put them
      // into feats
      feats[its[i].id] = new Object(); 

      // childs, is the children of the interior of the interior "iterframe"
      // ie, the relevant divs to loop through and look for
      // features and stems and such
      
      var childs = its[i].children[1].children[1].children;
      var fpat = new RegExp(its[i].id+"_feat[0-9]+$");
      var spat = new RegExp(its[i].id+"_stem[0-9]+$");
      for(var j=0; j<childs.length; j++){
        if(fpat.test(childs[j].id)){
          var fname = childs[j].children[1].children[0].value;
          var fvalue = childs[j].children[1].children[1].value;
          feats[its[i].id][fname]=fvalue;
        }
        // also, see if this lex-type has stems, and record that fact
        if(spat.test(childs[j].id) && childs[j].children[1].children[0].value != ""){
          feats[its[i].id]["has_stems"] = 1;
        }
      }
    }
  }

  // for each of these lex-types, see which one(s) don't
  // have any supertypes
  // 
  // there needs to be at least one of these, or else we can't connect to the root
  // also, every node needs a path to the root, but check this later
  var anchored = false;
  var ltsups = new Array(lts.length);
  for (var i =0; i<lts.length; i++){
    // there's some goofyness here where we need to discard the "name"
    // and only use the values in parans when something just got
    // input without being saved yet.
    //
    // basically, if we seen parans in the value, we'll assume this is the case
    var lsuper = document.getElementById(lts[i]+"_supertypes_multitext");
    if (lsuper.value == "") {
      anchored = true;
      ltsups[i] = type 
    } else if (lsuper.value.match(/\(.*\)?/)) {
      var tpat = RegExp(type+'[0-9]+','g');
      var matches = new Array();
      while (m = tpat.exec(lsuper.value)){
        matches[matches.length] = m;
      }
      ltsups[i] = matches.join(", "); 
    } else {
      ltsups[i] = lsuper.value;
    }
  }
  if (!anchored) {
    alert("Cannot visualize "+type+" hierarchy: at least one type must inherit from the generic "+type+"-lex. ie, not specify any supertypes in the questionnaire.");
    return null;
  }

  var g = new graph(type, lts, ltsups);

  // step through every type and add write down its inherited features
  // this pushes features down as far as possible
  for (var i = 0;i<lts.length; i++){
    var parents = new Array(); 
    var done = false;
    var nodes_seen = new Object();
    // have to stop once we've seen every node or
    // once all sts are the root 
    //
    // while we're at it, also check that every node has a path to the root
    // otherwise, cough up blood and die
    var root = false;
    parents = ltsups[i].split(", ");
    var paths = new Array(); 
    // seed paths with first gen parents
    for (var j=0;j<parents.length;j++){
      paths[paths.length] = [lts[i], parents[j]];
    }

    while (!done) {
      var new_parents = new Array();
      for (var j = 0; j< parents.length; j++){     
        for (f in feats[parents[j]]){
          if (f != "has_stems") {
            if (inherited_feats[lts[i]] == null ){
              inherited_feats[lts[i]] = new Object();
              inherited_feats[lts[i]][f]=feats[parents[j]][f];  
            } else if (inherited_feats[lts[i]][f] != null) { //might be inheriting a conflict
              if(inherited_feats[lts[i]][f] != feats[parents[j]][f]){
                inherited_feats[lts[i]][f]= "[conflict!] "+feats[parents[j]][f]+" && "+ inherited_feats[lts[i]][f];  
              } 
            } else {
              inherited_feats[lts[i]][f]=feats[parents[j]][f];  
            }
          }
        }

        if (parents[j] == type){
          root = true;
        }
        for (var r=0;r<paths.length;r++) {
          if (paths[r][paths[r].length-1] == parents[j]){
            var old_p = (paths.splice(r,1)).toString().split(",");
            for (k in g.bedges[parents[j]]) {
              var cycle = false;
              for (l=0;l<old_p.length;l++){
                if (k == old_p[l]){
                  alert("Cannot visualize; this hierarchy contains a cycle.  Found _"+k+"_ at multiple points inheritance path: ["+old_p+","+k+"]");
                  return;
                }               
              }
              var new_p = old_p.slice();
              new_p[new_p.length] = k;
              paths[paths.length] = new_p.slice(); 
              if (nodes_seen[k] == null){
                new_parents[new_parents.length] = k;
                nodes_seen[k] = 1;
              } 
            }
          }
        }
      }
      parents = new_parents.slice();
      if (parents.length == 0) {
        done = true;
      }
    }
    if (!root){
      alert("Cannot visualize "+type+" hierarchy: "+lts[i]+" isn't connected to the root.");
      return null;
    }
  }

  // some output
  var r;
  if(document.getElementById("result")){
    r = document.getElementById("result");
    r.style.display = "block";
    r.style.top=window.scrollY+20+"px"; // TJT 2014-09-05: Update top position
  } else {
    r = document.createElement("div");
    r.style.position="absolute";
    r.style.border="1px solid black";
    r.style.margin="20px";
    r.id="result";
    r.style.top=window.scrollY+20+"px"; // TJT 2014-09-05: Changing this to current scroll top + 20 margin
    r.style.left=0;
    r.style.background="white";
    r.style.padding="5px";
    r.style.height="80%";
    r.style.width="80%";
    r.style.zIndex="2"; // TJT 2014-09-05: Putting this on top of the background click out
    r.innerHTML+="<button onclick=\"remove_drawing()\">hide</button>";
    document.body.appendChild(r);
  }

  var c;
  if (document.getElementById("cnvs")){
    c = document.getElementById("cnvs");
  } else {
    c = document.createElement("canvas");
    c.id="cnvs";
    r.appendChild(c);
  }
 
  // TJT 2014-09-05: Add background element to remove drawing
  var background = document.createElement("div");
  background.id = "drawing_background";
  // Basic display properties to fill screen
  background.style.display = "block";
  background.style.position = "fixed";
  background.style.height = "100%";
  background.style.width = "100%";
  background.style.top = 0;
  background.style.left = 0;
  // Put behind drawing
  background.style.zIndex = "1";
  // Add event listener
  background.onclick = remove_drawing;
  document.body.appendChild(background);

  var h = r.clientHeight - 50;
  var w = r.clientWidth - 50;
  c.setAttribute('height', h);
  c.setAttribute('width', w);
  var ctx = c.getContext("2d"); 

  // sort out the vertices into 
  // "generations" 
  //
  // in the nodes_printed, store a max depth
  // where we keep pushing a node's generation
  // number deeper if it gets parents lower in the
  // graph.
  // this way the arrows always point downwardly
  var printed_all = false;
  var nodes_printed = new Object();
  var gens = new Array();
  var parents = new Array();
  var children = new Array();
  var n = 0;
  var total = 0;
  parents[0] = type;
  while (!printed_all) {
//    gens[n] = new Array();
    for (var i=0;i<parents.length;i++){ 
      for(var e in g.edges[parents[i]]){
        if (nodes_printed[e] == null){
          children[children.length] = e; 
          total++;
        } 
        nodes_printed[e]=n; //depth to appear at for node "e"
      }
    }
    parents = children.slice();
    children.length = 0;
    n++; //keeps track of how deep we are in "generations"
    if(total>=g.node_names.length){
      // push down children of the last generation
      for (var i=0;i<parents.length;i++){ 
        for(var e in g.edges[parents[i]]){
          nodes_printed[e]=n; //depth to appear at for node "e"
        }
      }
      printed_all = true;
    }
  }
  //
  //now build generations based on max depth
  for (t in nodes_printed){
    if(gens[nodes_printed[t]]==null){
      gens[nodes_printed[t]] = new Array();
    } 
    gens[nodes_printed[t]][gens[nodes_printed[t]].length] = t; 
  }
  

  // here we start drawing, 
  //
  // first, draw a "key" explaining the colors
  
  ctx.lineWidth=.5;
  ctx.strokeRect(2,8,340,54);
  ctx.font="10pt Arial";
  ctx.fillText("key:",10,20);
  ctx.font="Italic 9pt Arial";
  ctx.fillStyle="blue";
  ctx.fillText("- types with stems (instance types)", 12,32);
  ctx.font="9pt Arial";
  ctx.fillStyle="green";
  ctx.fillText("- inherited [ feature : value ] (only shown on instance types)",12,44);
  ctx.fillStyle="black";
  ctx.fillText("- [ feature : value ] defined on this type",12,56);
  ctx.lineWidth=1;
  
  // okay, done with the key  
  //
  //
  var v_positions = new Object();
  var ydelta = Math.floor((h-20)/(gens.length+1));
  
  // draw the root node
  ctx.font="Italic 12pt Arial";
  ctx.fillStyle="black";
  ctx.fillText(type+"-lex", Math.floor(w*.5)- 10 ,15);
  v_positions[type] = new Array(Math.floor(w*.5),20);

  // draw everything else
  for(var i=0;i<gens.length;i++){
    var xdelta = Math.floor(w/(gens[i].length+1));

    // if there's too many in a gen, the we'll split it in half and
    // stagger them up and down
    var stagger = false;
    if (gens[i].length > 8) {
      stagger = true;
    }

    for(var j=0;j<gens[i].length;j++){
      // see if there's a user defined name and print that
      // if this type has stems, we print in blue
      if(feats[gens[i][j]]["has_stems"] == 1){
        ctx.fillStyle="blue";
      } else {
        ctx.fillStyle="black";
      }

      var text = document.getElementsByName(gens[i][j]+"_name")[0].value;
      if (text == ""){
        text+=gens[i][j];
      }

      // 10+ on x value moves the type name out of the way of the
      // graph vertex
      
      if(stagger && (j % 2)){
        ctx.fillText(text,10+ ((j+1)*xdelta)-(.5*xdelta),((i+1)*ydelta)+(.5*ydelta));
        v_positions[gens[i][j]] = new Array(((j+1)*xdelta)-(.5*xdelta),((i+1)*ydelta)+(.5*ydelta)) 
      } else {
        ctx.fillText(text,10+ ((j+1)*xdelta)-(.5*xdelta),((i+1)*ydelta));
        v_positions[gens[i][j]] = new Array(((j+1)*xdelta)-(.5*xdelta),((i+1)*ydelta)) 
      }
      
      // now print features and values just under types
      // for blue guys (types with stems), also print inherited feats
      var z=0;
      ctx.font="10pt Arial";
      var matrix_wid = 0;
      var text = "";
      for (f in feats[gens[i][j]]){
        if (f == "has_stems" && feats[gens[i][j]][f] == 1){
          ctx.fillStyle="green";
          for (h in inherited_feats[gens[i][j]]){
            z++; 
            text=h+" : "+inherited_feats[gens[i][j]][h];  
            if(stagger && (j %2)){
              ctx.fillText(text,12+((j+1)*xdelta)-(.5*xdelta),(15*z)+((i+1)*ydelta)+(.5*ydelta));
            } else {
              ctx.fillText(text,12+((j+1)*xdelta)-(.5*xdelta),(15*z)+((i+1)*ydelta));
            }
            var tw = ctx.measureText(text).width + 15;
            matrix_wid = matrix_wid < tw ? tw : matrix_wid;
          }
        } else {
          ctx.fillStyle="black";
          z++; 
          text=f+" : "+feats[gens[i][j]][f];  
          var tw = ctx.measureText(text).width + 15;
          matrix_wid = matrix_wid < tw ? tw : matrix_wid;
          if(stagger && (j %2)){
            ctx.fillText(text,12+((j+1)*xdelta)-(.5*xdelta),(15*z)+((i+1)*ydelta)+(.5*ydelta));
          } else {
            ctx.fillText(text,12+((j+1)*xdelta)-(.5*xdelta),(15*z)+((i+1)*ydelta));
          }
        } 
      }
      // draw the brackets
      if (z > 0){
        var localy; 
        if (stagger && (j%2)){
          localy = 4+((i+1)*ydelta+(.5*ydelta));
        } else {
          localy = 4+((i+1)*ydelta);
        }
        ctx.beginPath();
        ctx.moveTo(8+((j+1)*xdelta)-(.5*xdelta),localy);
        ctx.lineTo(4+((j+1)*xdelta)-(.5*xdelta),localy);
        ctx.lineTo(4+((j+1)*xdelta)-(.5*xdelta),(15*z)+localy);
        ctx.lineTo(8+((j+1)*xdelta)-(.5*xdelta),(15*z)+localy);
        ctx.stroke();

        ctx.beginPath();
        ctx.moveTo(4+matrix_wid+((j+1)*xdelta)-(.5*xdelta),localy);
        ctx.lineTo(8+matrix_wid+((j+1)*xdelta)-(.5*xdelta),localy);
        ctx.lineTo(8+matrix_wid+((j+1)*xdelta)-(.5*xdelta),(15*z)+localy);
        ctx.lineTo(4+matrix_wid+((j+1)*xdelta)-(.5*xdelta),(15*z)+localy);
        ctx.stroke();
      }
      ctx.font="Italic 12pt Arial";
    }
  }

  //now draw all the edges
  ctx.lineWidth=1;
  ctx.lineCap="round";
  ctx.strokeStyle="#888888";
  for (e in g.edges[type]){
      ctx.beginPath();
      ctx.moveTo(v_positions[type][0],v_positions[type][1]);
      ctx.lineTo(v_positions[e][0],v_positions[e][1]);
      ctx.stroke();
  }
  for (var i=0;i<g.node_names.length;i++){
    for (e in g.edges[g.node_names[i]]){
      ctx.beginPath();
      ctx.moveTo(v_positions[g.node_names[i]][0],v_positions[g.node_names[i]][1]);
      ctx.lineTo(v_positions[e][0],v_positions[e][1]);
      ctx.stroke();
    }
  }
}

function graph(r, nts, ntsups) { 
  this.root = r;
  this.node_names = nts; 
  this.edges = new Object();
  this.bedges = new Object(); //backwards edges (for finding parents)
  
  for (var i=0;i<nts.length;i++){
    var sts = ntsups[i].split(", ");
    for (var j=0;j<sts.length;j++){
      if(this.edges[sts[j]]== null){
        this.edges[sts[j]] = new Object(); 
      }
      if(this.bedges[nts[i]]== null){
        this.bedges[nts[i]] = new Object(); 
      }
      this.edges[sts[j]][nts[i]] = 1;  
      this.bedges[nts[i]][sts[j]] = 1;  
    }
  }
}

function remove_drawing() {
  var d = document.getElementById("result");
  d.style.display="none";
  var b = document.getElementById("drawing_background");
  b.parentNode.removeChild(b);
  return;
}
