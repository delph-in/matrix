// $Id: $

//////////////////////////////////////////////////////////////////////
// Utility functions

// HACK: Force IE to re-layout a paragraph
function force_layout(para)
{
  var old_h = para.style.height;
  para.style.height = 'auto';
  para.style.height = '';
  para.style.height = old_h;
}

// HACK: IE doesn't have the indexOf method on arrays
function indexOf(array, value)
{
  for (var i = 0; i < array.length; i++) {
    if (array[i] == value) {
      return i;
    }
  }

  return -1
}

// HACK: IE disagrees about where the event.target lives
function event_target(e)
{
  if (!e) e = window.event;
  if (e.target) return e.target;
  return e.srcElement;
}

// HACK: IE uses a different name for the cssRules array in styleSheets
function get_css_rule(name)
{
  var r = document.styleSheets[0].cssRules;
  if (!r) r = document.styleSheets[0].rules;
  for (var i = 0; i < r.length; i++) {
    if (r[i].selectorText == name) {
      return r[i];
    }
  }
  return null;
}

// For logging debugging info to the end of the page
function timestamp(s)
{
  var d = document;
  var curTime = new Date();
  d.body.appendChild(d.createTextNode(s + ':\t' + curTime.getTime() / 1000));
  d.body.appendChild(d.createElement('br'));
}

// Returns true iff the node and all its ancestors are NOT display:none
function is_displayed(node)
{
  while (node && node.style) {
    if (node.style.display == 'none') {
      return false;
    }
    node = node.parentNode;
  }

  return true;
}

//////////////////////////////////////////////////////////////////////
// Main Page functions

// toggle_display()
// Handle a click on a section arrow on the main page
function toggle_display(para_id, button_id)
{
  p = document.getElementById(para_id);
  b = document.getElementById(button_id);
  if (p.style.display == 'none') {
    p.style.display = 'block';
    b.innerHTML = '&#9660;';
  } else {
    p.style.display = 'none';
    b.innerHTML = '&#9658;';
  }
}

//////////////////////////////////////////////////////////////////////
// Sub Page functions

// clear_form()
// Set the values of all form fields to the empty string
function clear_form()
{
  var elements = document.getElementsByTagName('input');
  for (var i = 0; elements.item(i); i++) {
    var elm = elements[i];
    if (elm.type == 'text') {
      elm.value = '';
    } else if (elm.type == 'radio' || elm.type == 'checkbox') {
      elm.checked = ''
    }
  }
  elements = document.getElementsByTagName('select');
  for (var i = 0; elements.item(i); i++) {
    elements[i].value = '';
  }
}

// save_form()
// Save and Vivify the choices on the current subpage
// Vivify --> Validate? --JDC 10feb2012
function save_form(section)
{
  var elm = document.getElementsByTagName('form')[0];
  var inp = document.createElement('input');
  inp.type = "hidden";
  inp.name="subpage";
  inp.value=section;
  elm.appendChild(inp);
  elm.submit();
}

// submit_main()
// Submit the form and return to the main page
function submit_main()
{
  var form = document.getElementsByTagName('form')[0];
  var elms = document.getElementsByTagName('input');
  for (var i = 0; i < elms.length; i++) {
    if (elms[i].name == "subpage") {
      form.removeChild(elms[i]);
    }
  }
  form.submit();
}

// submit_go(subpage)
// Submit the form and go to another subpage 
function submit_go(subpage){
  var form = document.getElementsByTagName('form')[0];
  var elms = document.getElementsByTagName('input');
  for (var i = 0; i < elms.length; i++) {
    if (elms[i].name == "subpage") {
      form.removeChild(elms[i]);
    }
  } 
  var inp = document.createElement('input');
  inp.type= "hidden";
  inp.name= "subpage";
  inp.value=subpage
  form.appendChild(inp);
  form.submit();
}


// toggle_display_lex()
// Handle a click on a section show/hide button on the Lexicon Page
function toggle_display_lex(element_id, button_id)
{
  p = document.getElementById(element_id);
  text_boxes = document.getElementsByName(element_id+'_name');
  errs = document.getElementById(element_id+"_errors");
  n = "";
  if (text_boxes.length > 0) {
    n = text_boxes[0].value;
  }
  b = document.getElementById(button_id);
  if (p.style.display == 'none') {
    p.style.display = 'block';
    if (errs != null) {
      errs.style.display = 'none';
    }
    if (n == "") {
      b.innerHTML = '&#9660; '+n+element_id + '<br />';
    } else {
      b.innerHTML = '&#9660; '+n+' ('+element_id+')<br />';
    }
    //    document.cookie = element_id+"=block";
    //    document.cookie = button_id+"=block";
  } else {
    p.style.display = 'none';
    if (errs != null) {
      errs.style.display = '';
    }
    if (n == "") {
      b.innerHTML = '&#9658; '+n+element_id + '<br />';
    } else {
      b.innerHTML = '&#9658; '+n+' ('+element_id+')<br />';
    }
    //    document.cookie = element_id+"=none";
    //    document.cookie = button_id+"=none";
  }
}

// toggle_all_display_lex()
// Handle a click on the show/hide all button on the lexicon page
function toggle_all_display_lex(on)
{
  iters = document.getElementsByClassName('iterator');
  all_button = document.getElementById('toggle_all_lex_button');
  if (on==1)
  {
    all_button.attributes.onclick.nodeValue = 'toggle_all_display_lex(0)';
    all_button.innerHTML = '&#9658; all sections';
  } else {
    all_button.attributes.onclick.nodeValue = 'toggle_all_display_lex(1)';
    all_button.innerHTML = '&#9660; all sections';
  }
  for (var x=0; x<iters.length; x++)
  {
    iter = iters[x];
    if(iter.id.search('TEMPLATE')==-1) { //don't mess with display of TEMPLATES
      button = document.getElementById(iter.id+'button');
      if (button != null) {
	if(on==1){
	  if(iter.style.display == 'block' || iter.style.display == '')
	    toggle_display_lex(iter.id, button.id);
        } else {
          if(iter.style.display == 'none' || iter.style.display == '')
            toggle_display_lex(iter.id, button.id);
	}
      }
    }
  }
}

// fill_display_name()
// Used to fill the name of the show/hide label after editing the
// name text field.
function fill_display_name(id)
{
  var elm = document.getElementById(id+'button');
  var name = document.getElementsByName(id+'_name')[0].value;
  if (name != "") {
    elm.innerHTML = '&#9660; '+name + ' ('+id+')';
  } else {
    elm.innerHTML = '&#9660; '+id;
  }
}

// focus_all_fields()
// Pass through the document once, giving the focus to every form
// field.  This allows all the auto-resizing items a chance to
// calculate their size.
function focus_all_fields()
{
  // Surrounded by try...catch because IE doesn't like to give focus to
  // undisplayed form elements.
  try {
    var f = document.forms[0];
    if (f) {
      var e = f.elements;
      for (var i = 0; i < e.length; i++) {
        // fill_regex can be very slow, so don't do it at load time
        if (e[i].onfocus &&
            e[i].onfocus.toString().search(/fill_regex/) == -1) {
          e[i].onfocus();
        }
      }
    }
  } catch (err) {
  }
}

//////////////////////////////////////////////////////////////////////
// Animation functions

// Animations are used to modify the web page smoothly rather than
// instantaneously.
var animations = [];
var an_max = 0;
var blink_count = 0;
function animate()
{
/*
  blink_count++;
  var r = get_css_rule('.error');
  if (r) {

    var bcm = blink_count % 100;
    var br = 20;
    if (bcm < br) {
      r.style.opacity = (br - bcm) / br;
    } else if (bcm < 2 * br) {
      r.style.opacity = (bcm - br) / br;
    } else {
      r.style.opacity = 1;
    }
  }
*/

  for (var i = an_max - 1; i >= 0; i--) {
    var a = animations[i];
    var n = document.getElementById(a.id);
    if (a.ticks) {
      var v = Number(n.style[a.property].replace(/px/, ''));
      if (a.factor > 0) {
        v *= a.factor;
      } else {
        v += a.step;
      }
      n.style[a.property] = v + 'px';
      a.ticks--;
    } else {
      n.style[a.property] = '';
      //n.style.overflow = '';
      animations =
        animations.slice(0,i).concat(animations.slice(i + 1, an_max));
      an_max--;
    }
  }

  setTimeout('animate()', 10);
}

// expand_region()
// Begin an animation that expands an HTML element gracefully
function expand_region(id)
{
  var n = document.getElementById(id);

  n.style.overflow = 'hidden';

  var a = { id: id, property: 'maxHeight', factor: 2, ticks: 10 };
  n.style[a.property] = '1px';

  animations[an_max++] = a;
}

//////////////////////////////////////////////////////////////////////
// Iterator functions

// prev_div()
// Find and return the DIV immediately before a tag, if there is one and
// it's not a template or anchor DIV from an iterator.
function prev_div(n, name)
{
  var p = n.previousSibling;
  while (p && p.tagName != 'DIV') {
    p = p.previousSibling;
  }

  if (p.id == name + '_TEMPLATE' || p.id == name + '_ANCHOR')
    return null;
  else
    return p;
}

// do_clone_region()
// Worker function that clones the invisible tempate of an iterator,
// replaces any iterator variables with the proper values, and inserts
// the copy into the page.
function do_clone_region(id, iter_var, bAnim, bShow)
{

  var d = document.getElementById(id + '_TEMPLATE');
  var a = document.getElementById(id + '_ANCHOR');
  var p = prev_div(a, id);

  var cur = 1;
  if (p && p.id) {
    var pid = p.id;
    if (pid.indexOf(id) == 0) {
      pid = pid.slice(id.length);
      var i = pid.search(/[0-9]+/);
      if (i != -1) {
        cur = Number(pid.slice(i));
        cur++;
      }
    }
  }

  var n = d.cloneNode(true);

  var re = new RegExp('{' + iter_var + '}', 'g');
  n.innerHTML = n.innerHTML.replace(re, cur);

  n.id = id + cur;
  n.style.display = '';

  // only add the show/hide button on iterators which ask for
  // a show/hide button
  if (bShow){
    var b = document.createElement("a");
    b.id = n.id+'button';
    b.innerHTML = '&#9660; '+n.id + '<br />';
    b.setAttribute('onclick', "toggle_display_lex('"+n.id+"', '"+n.id+"button')");
    a.parentNode.insertBefore(b, a);
  }

  a.parentNode.insertBefore(n, a);

  multi_init();

  if (bAnim) {
    expand_region(n.id);
  }
}

// clone_region()
// Clone a region and expand using animation
function clone_region(id, iter_var, bShow)
{
    do_clone_region(id, iter_var, true, bShow);
}

// clone_region()
// Clone a region and expand *without* animation
function clone_region_noanim(id, iter_var)
{
  do_clone_region(id, iter_var, false);
}

// remove_region()
// Remove the last item in an iterator with id
function remove_region(id)
{
  var a = document.getElementById(id + '_ANCHOR');
  var p = prev_div(a, id);
  
  if (p && p.id) {
    if (p.id.indexOf(id) == 0) {
      p.parentNode.removeChild(p);
    }
  }
}

// remove_element()
// Remove the element with id
// and remove any associated show/hide button
// and remove any show/hide errors
function remove_element(id)
{
  var e = document.getElementById(id);
  e.parentNode.removeChild(e);
  var b = document.getElementById(id+'button');
  var err = document.getElementById(id+'_errors');
  if (b != null){
      b.parentNode.removeChild(b);
  }
  if (err != null){
      err.parentNode.removeChild(err);
  }
}

//////////////////////////////////////////////////////////////////////
// SELECT filler functions

// Remove the auto-filled OPTIONs of a SELECT
function remove_temp_options(select)
{
  for (var i = select.options.length - 1; i >= 0; i--) {
    var o = select.options[i];
    if (o.className == 'temp') {
      select.removeChild(o);
    }
  }
}

// Set the value of a SELECT, adding a temporary OPTION if necessary
function set_select_value(select, value, text)
{
  // Check to see if the SELECT already has an OPTION for value
  var ops = select.options;
  for (var i = 0; i < ops.length; i++) {
    if (ops[i].value == value) {
      break;
    }
  }
  if (i == ops.length) { // wasn't handled above, so insert an option
    var o = document.createElement('option');
    o.className = 'temp';
    o.value = value;
    o.innerHTML = text;
    select.appendChild(o);
  }

  select.value = value;
}

// fill()
// there is one function to fill a SELECT, and it takes an array of options
// to insert, and manages the re-selecting of previously selected items
function fill(name, items)
{
  var select = document.getElementsByName(name)[0];
  var old_val = select.value;  // store the previously selected option
  var old_text = old_val;
  if (select.selectedIndex != -1) {
    old_text = select.options[select.selectedIndex].innerHTML;
  }
  remove_temp_options(select);

  for (var i = 0; i < items.length; i++) {
    // Each item should be a (string, value) pair
    var o = document.createElement('option');
    o.className = 'temp';
    o.value = items[i][1];
    o.innerHTML = items[i][0];
    select.appendChild(o);
  }
  
  set_select_value(select, old_val, old_text);
  force_layout(select.parentNode);
}

// fill_regex() Return the values of any form fields on the page whose NAME
// matches the pattern.  If the nameOnly flag is true, make the OPTION's VALUE
// attribute equal to its contents.
function fill_regex(pattern, nameOnly)
{
  pattern = '^' + pattern + '$';
  var items = new Array();

  // Pass through the form fields in the page, looking for ones whose
  // name attribute matches the pattern.  When one is found, use its
  // contents to create an option.
  var e = document.forms[0].elements;
  for (var i = 0; i < e.length; i++) {
    if (e[i].name.search(pattern) != -1) {
      var val = e[i].name.replace(/_[^_]*$/, '');
      var desc = val;
      var f = document.getElementsByName(val + '_name');
      if (f && f[0] && f[0].value) {
        if (nameOnly) {
          val = desc = f[0].value;
        } 
	else {
          desc = f[0].value + ' (' + desc + ')';
        }
      }
      items.push([desc, val]);
    }
  }
  return items
}

// fill_feature_names()
// Return items from the array features[], where every OPTION is a feature name.
// The cat(egory) argument allows you to restrict the features by category
function fill_feature_names(cat)
{
  var items = new Array()
  for (var i = 0; i < features.length; i++) {
    var f = features[i].split(':');
    
    if (typeof(cat) == "undefined" ||
        f[2] == cat || f[2] == 'both' || cat == 'both') {
      items.push([f[0], f[0]]);
    }
  }
  return items
}

// fill_feature_values()
// Return items from the array features[], where every OPTION is a feature
// value for the feature named by the form element named other_name.
function fill_feature_values(other_name, literal_feature)
{
  var items = new Array()
  if (literal_feature == 1) { var other_val = other_name; }  
  else { var other_val = document.getElementsByName(other_name)[0].value; }

  for (var i = 0; i < features.length; i++) {
    var v = features[i].split(':');

    if (v[0] == other_val) {
      v = v[1].split(';');

      for (var j = 0; j < v.length; j++) {
        var n = v[j].split('|');
        items.push([n[1],n[0]])
      }
    }
  }
  return items
}

// fill_case_patterns()
// Return items from either array morph_case_patterns or verb_case_patterns, as
// determined by the morph argument.
function fill_case_patterns(morph)
{
  var items = new Array();
  var pats = (morph) ? morph_case_patterns : verb_case_patterns;
  for (var i = 0; i < pats.length; i++) {
    var p = pats[i].split(':');
    items.push([p[1], p[0]]);
  }
  return items
}

// fill_numbers()
// Return items from the array numbers[], where every OPTION is a value of the
// number feature.
function fill_numbers(select_name)
{
  var items = new Array();
  for (var i = 0; i < numbers.length; i++) {
    var n = numbers[i].split(':');
    items.push([n[0],n[0]]);
  }
  return items
}

// fill_cache()
// Return items from the given cache.
function fill_cache(cache_name)
{
  var cache = window[cache_name];
  var items = new Array();
  for (var i = 0; i < cache.length; i++) {
    var x = cache[i].split(':');
    items.push([(x[0] != '') ? x[0] + ' (' + x[1] + ')' : x[1], x[1]]);
  }
  return items
}

//////////////////////////////////////////////////////////////////////
// Multi-SELECT functions
//
// To implement multi-select dropdown controls, we make the associated
// SELECT controls display:none, then insert immediately after them a
// text control and a button that simulates a dropdown.  When either is
// clicked, a box containing a list of checkboxes and labels appears
// below, until the user clicks the fake-select control again.
//
// The functions below are passed the name of the undisplayed SELECT.
// From this, it is possible to derive the IDs of the text and button
// controls by appending '_multitext' and '_multibutton',
// respectively, to the NAME of the SELECT.  (It's important that they
// have no NAME attribute of their own so they don't get submitted as
// form results.)  The temporary box's ID can be similarly derived by
// appending '_multibox'.

// multi_init()
// Pass through the page, and for each SELECT that has CLASS=multi,
// create a multi-SELECT.
function multi_init()
{
  var selects = document.getElementsByTagName('select');
  for (var i = 0; i < selects.length; i++) {
    var s = selects[i];
    if (s.className == 'multi' && is_displayed(s)) {
      multi_create(s);
    }
  }
}

// multi_create()
// Take the SELECT control passed in, make it display:none, and insert
// the text field and button that will replace it.
function multi_create(select)
{
  select.style.display = 'none';
  select.onfocus();

  var t = document.createElement('input');
  t.id = select.name + '_multitext';
  t.type = 'text';
  if (select.selectedIndex != -1) {
    t.value = select.options[select.selectedIndex].innerHTML;
  }
  t.size = Math.max(10, t.value.length);
  t.readonly = 'readonly';
  t.onmousedown = function(e) {
    var b_id = event_target(e).id.replace(/_multitext/, '_multibutton');
    document.getElementById(b_id).onclick();
  }
  t.onkeypress = function(e) { return multi_keypress(e, select.name); }
  select.parentNode.insertBefore(t, select.nextSibling);

  var b = document.createElement('input');
  b.type = 'button';
  b.id = select.name + '_multibutton';
  b.value = '\u25BC';
  b.style.paddingLeft = b.style.paddingRight = '0px';
  b.onclick = function() {
    multi_click(select.name);
  }
  select.parentNode.insertBefore(b, t.nextSibling);
}

// multi_box()
// Find and return the box associated with a multi-SELECT control.  The
// box should only exist if the multi-SELECT is in the open state.
function multi_box(select_name)
{
  return document.getElementById(select_name + '_multibox');
}

// multi_back()
// Find and return the background associated with a multi-SELECT
// control.  The background should only exist if the multi-SELECT is
// in the open state.
function multi_back(select_name)
{
  return document.getElementById(select_name + '_multiback');
}

// multi_open()
// Create and fill the drop-down box for a multi-SELECT control.
function multi_open(select_name)
{
  var select = document.getElementsByName(select_name)[0];
  var text = document.getElementById(select_name + '_multitext');

  // Create the box, a SPAN tag
  var span = document.createElement('span');
  span.id = select.name + '_multibox';
  span.style.position = 'absolute';
  span.style.zIndex = 2;
  span.style.left = text.offsetLeft + "px";
  span.style.top = text.offsetTop + text.offsetHeight + "px";
  span.style.minWidth = text.offsetWidth - 6 + "px";
  span.style.left = text.offsetLeft;
  span.style.top = text.offsetTop + text.offsetHeight;
  span.style.minWidth = text.offsetWidth - 6;
  span.style.backgroundColor = 'white';
  span.style.border = '1px solid black';
  span.style.padding = '2px 4px 2px 2px';

  // Populate the box
  var vals = select.value.split(', ');
  select.onfocus();
  var ops = select.options;
  for (var i = 0; i < ops.length; i++) {
    if (ops[i].value.length && ops[i].value.indexOf(', ') == -1) {
      if (span.firstChild) {
        span.appendChild(document.createElement('br'));
      }

      var check = document.createElement('input');
      check.type = 'checkbox';
      check.id = select_name + '__' + ops[i].value;
      check.onkeypress = function(e) { return multi_keypress(e, select.name); }
      span.appendChild(check);
      if (indexOf(vals, ops[i].value) != -1) {
        check.checked = true;  // Must be done AFTER insertion on IE
      }

      var label = document.createTextNode(' ' + ops[i].innerHTML);
      span.appendChild(label);
    }
  }

  text.parentNode.insertBefore(span, text);

  // Create the background, an invisible SPAN tag that covers the whole
  // page and captures any mouse clicks outside the box.
  span = document.createElement('span');
  span.id = select.name + '_multiback';
  span.style.position = 'absolute';
  span.style.zIndex = 1;
  span.style.left = 0 + "px";
  span.style.top = 0 + "px";
  span.style.width = document.body.scrollWidth + "px";
  span.style.height = document.documentElement.scrollHeight + "px";
  span.style.backgroundColor = 'white';
  span.style.opacity = 0;
  span.style.filter = 'alpha(opacity=0)';
  span.onclick = function() {
    multi_click(select.name);
  }

  text.parentNode.insertBefore(span, text);
}

// multi_close()
// Copy the state of the drop-down box into the SELECT control, then
// delete the box.
function multi_close(select_name)
{
  var box = multi_box(select_name);
  var back = multi_back(select_name);
  var select = document.getElementsByName(select_name)[0];
  var ops = select.options;

  // Concatenate the selected checkboxes into a comma-separated string
  var ivalue = '';   // internal value
  var fvalue = '';   // friendly value
  for (var check = box.firstChild; check; check = check.nextSibling) {
    if (check.tagName == 'INPUT' && check.checked) {
      if (ivalue.length) {
        ivalue += ', ';
        fvalue += ', ';
      }

      var v = check.id.replace(/.*__/, '');
      ivalue += v;

      for (var i = 0; i < ops.length; i++) {
        if (v == ops[i].value) {
          v = ops[i].innerHTML;
        }
      }

      fvalue += v;
    }
  }

  set_select_value(select, ivalue, fvalue);

  var text = document.getElementById(select_name + '_multitext');
  text.value = fvalue;
  text.size = Math.max(10, text.value.length);

  box.parentNode.removeChild(box);
  back.parentNode.removeChild(back);
}

// multi_click()
// Handle a click on the multi-SELECT button.
function multi_click(select_name)
{
  var box = multi_box(select_name);
  if (box) {
    multi_close(select_name);
  } else {
    multi_open(select_name);
  }
}

// multi_keypress()
// Handle the ENTER key by swallowing it and closing the box
function multi_keypress(e, select_name)
{
  var keynum;
  if (!e) {
    keynum = window.event.keyCode;
  } else {
    keynum = e.which;
  }

  if (keynum == 10 || keynum == 13) {
    multi_click(select_name);
    return false;  // prevent further processing
  }

  return true;  // allow further processing
}

////////////////////////////////////////////////////////////
// Auto-Text filling Functions
////////////////////////////////////////////////////////////

// Fill pred elements with values based on the orth element
function fill_pred(name,pos)
{
  var elms = document.getElementsByName(name+'_orth');
  var word = '';
  for (var i = 0; i < elms.length; i++) {
    if (elms[i].type == "text") {
      word = elms[i].value;
    }
  }
  var pred = "_"+word+"_"+pos+"_rel";
  elms = document.getElementsByName(name+'_pred');
  for (var i = 0; i < elms.length; i++) {
    if (elms[i].type == "text" && elms[i].value == '' && word != '') {
      elms[i].value = pred;
      var text_elms = document.getElementsByTagName('input');
      var match_inds = [];
      for (var j = 0; j < text_elms.length; j++) {
        if (text_elms[j].type == "text" && text_elms[j].value.match(new RegExp("^_"+word+"_"+pos+"_?[0-9]*_rel$",""))){
          match_inds.push(j);
        }  
      }
      if (match_inds.length > 1){
        for (var j = 0; j < match_inds.length; j++) {
          text_elms[match_inds[j]].value = pred.replace("_rel", "_"+(j+1)+"_rel");
        }
      }
    }
  }
}

function aux_fill_pred(name, stem, pos)
{
  var elms = document.getElementsByName(name+"_sem");
  for (var i = 0; i < elms.length; i++) {
    if (elms[i].value == "add-pred" && elms[i].checked) {
      fill_pred(name+"_"+stem, pos);
    }
  }
}

// fill_hidden_errors()
// This moves errors which are not displayed to the outside of
// show/hide button. It should be called onload and no where else.
function fill_hidden_errors()
{
  var hash = window.location.hash;
  var elms = document.getElementsByTagName("div");
  for (var i = 0; i < elms.length; i++) {
    if (elms[i].className == "iterator") {
      fill_errors(elms[i].id);
    }
  }
  if (hash != "") {
    window.location.hash = hash;
  }
}

// fill_errors(id)
// Moves the errors for a interator div to the associated error 
// span outside of the show hide (if such a span exists.
// This should only be called by the fill_hidden_errors function.
function fill_errors(element_id)
{
  
  var s = document.getElementById(element_id+'_errors');
  var p = document.getElementById(element_id);
  var anchors = p.getElementsByTagName('a');
  for (var i = 0; i < anchors.length; i++) {
    if (s != null && anchors[i].name.match( new RegExp("_error$|_warning$"))) {
      s.appendChild(anchors[i].cloneNode(true));
    }
  }
}

//import_toolbox_lexicon
function import_toolbox_lexicon()
{
  var elm = document.getElementsByTagName('form')[0];
  var inp = document.createElement('input');
  inp.type = "hidden";
  inp.name = "import_toolbox";
  inp.value = "true";
  elm.appendChild(inp);
  submit_main();
}
////////////////////////////////////////////////////////////
// Special functions for Sentential Negation Subpage 
////////////////////////////////////////////////////////////

// set_negexp(n)
//
function set_negexp(n)
{
  var value = n;
  var divs = document.getElementsByClassName("neg_exp_switch");
	for(var i=0; i<divs.length;i++){
    var d = divs[i];
    d.style.display = 'none';
	}
  var d;
  switch (n){
    case '0':
      var d = document.getElementById('zero-neg');
      break;
    case '1':
      var d = document.getElementById('uni-neg');
      break
    case '2':
      var d = document.getElementById('bi-neg');
      break;
    case '3':
      var d = document.getElementById('tri-neg');
      break;
    case 'more':
      var d = document.getElementById('x-neg');
      break;
    default:
      var d = null; 
  }
  if (d != null)
  {
    d.style.display ='block';
  }
}

function set_negmorph(t1,t2){

  // first we hide everything
  var divs = document.getElementsByClassName("neg"+1+"_switch");
	for(var i=0; i<divs.length;i++){
    divs[i].style.display = 'none';
	}

  // now calculate the bipartite negation type
  var t;
  if (t1=='b'){
    if (t2=='b'){
      t = 'infl-infl'; 
    } else if (t2 == 'fh') {
      t = 'infl-head'; 
    } else if (t2 == 'fc') {
      t = 'infl-comp'; 
    } else if (t2 == 'fm') {
      t = 'infl-mod'; 
    } else {
      t = 'default';
    }
  } else if (t1 == 'fh'){
    if (t2=='b'){
      t = 'infl-head'; 
    } else if (t2 == 'fh') {
      t = 'head-head'; 
    } else if (t2 == 'fc') {
      t = 'head-comp'; 
    } else if (t2 == 'fm') {
      t = 'head-mod'; 
    } else {
      t = 'default';
    }
  } else if (t1 == 'fc'){
    if (t2=='b'){
      t = 'infl-comp'; 
    } else if (t2 == 'fh') {
      t = 'head-comp'; 
    } else if (t2 == 'fc') {
      t = 'comp-comp'; 
    } else if (t2 == 'fm') {
      t = 'comp-mod'; 
    } else {
      t = 'default';
    }
  } else if (t1 == 'fm'){
    if (t2=='b'){
      t = 'infl-mod'; 
    } else if (t2 == 'fh') {
      t = 'head-mod'; 
    } else if (t2 == 'fc') {
      t = 'comp-mod'; 
    } else if (t2 == 'fm') {
      t = 'mod-mod'; 
    } else {
      t = 'default';
    }
  }
 

  // now set up the page accordingly
  var d = document.getElementById('bineg_fb');
  switch(t){
    case 'infl-infl':
      d.innerHTML='infl-infl';
      break;
    case 'infl-head':
      d.innerHTML='infl-head';
      break;
    case 'infl-comp':
      d.innerHTML='infl-comp';
      break;
    case 'infl-mod':
      d.innerHTML='infl-mod';
      break;
    case 'head-head':
      d.innerHTML='head-head';
      break;
    case 'head-comp':
      d.innerHTML='head-comp';
      break;
    case 'head-mod':
      d.innerHTML='head-mod';
      break;
    case 'comp-comp':
      d.innerHTML='comp-comp';
      break;
    case 'comp-mod':
      d.innerHTML='comp-mod';
      break;
    case 'mod-mod':
      d.innerHTML='mod-mod';
      break;
    default:
      d.innerHTML='choose a morpheme type for each exponent';
      break;
  }
  /*var d;
  // n is which morpheme we're specifying (neg1 or neg2)
  // o is an option on bipartite negation, a morph type { b, fh, fd }
  // if we're dealing with neg1, we set soem restrictions on neg2 
  // type depending on what was selected in neg1
  // if we're dealing with neg2, we may offer some advice depending
  // on their choice for neg1
  // we set 'd' to the section we want to dipslay, if it's null
  // after we loop through, we don't do anything.

  // basically, the user might be changing neg1 or neg2, so we
  // have to set the messages accordingly for both cases
  if(n=='1'){
    switch (o){
      case 'b':
        // enable infl-neg, which allows the lexicon features
        document.forms["choices_form"]["infl-neg"].checked=true;
        document.forms["choices_form"]["neg2-type"][0].disabled=false;
        document.forms["choices_form"]["neg2-type"][1].disabled=false;
        document.forms["choices_form"]["neg2-type"][2].disabled=false;
        d = document.getElementById('neg'+n+'-b');
        // if neg2 is bound, then we'll put our advice about circumfixes
        if (document.forms["choices_form"]["neg2-type"][0].checked) {
          var d2 = document.getElementById("neg2-b");
          var chkd = document.forms["choices_form"]["neg1b-neg2b"].checked; 
          var ibox;
          if (chkd) {
            ibox =  "<input type=\"checkbox\" name=\"neg1b-neg2b\" checked=\""+chkd+"\">NEG1 bound to Aux requires NEG2 bound to Lexical Verb</input>";
          } else {
            ibox =  "<input type=\"checkbox\" name=\"neg1b-neg2b\">NEG1 bound to Aux requires NEG2 bound to Lexical Verb</input>";
          }
          d2.innerHTML = "<p>If both NEG1 and NEG2 are bound to the same root, you can set up the dendency using the morphotactics system on the lexicon page. Only specify that one of your LRIs is 'negation plus' (you only need one <em>neg_rel</em>), and set up a requires relation between the two morphemes.</p><p>If NEG1 is bound to an auxiliary and NEG2 is bound to a lexical verb, check the box below.  This will enable several options for you as you define your lexical rules for NEG1 and NEG2 on the morphotactics page.  A value for FORM 'negform' will be added to your grammar (as a subtype of nonfinite).  Indicate the lexical rule corresponding to NEG1 by setting [NEGATION +] (this adds the negative semantics) and the requirement that the complement be [FORM negform].  Likewise, indicate NEG2 by selecting [NEG2 +].  This will set up the NEG2 lexical rule to change the FORM value on its head to negform.  In this way, auxiliary verbs inflected by NEG1 will require their complement to be headed by a verb which has been inflected by NEG2.</p>"+ibox; 
        } else if (document.forms["choices_form"]["neg2-type"][1].checked){
          var d2 = document.getElementById("neg2-fh");
          d2.innerHTML = "<p>NEG1 is bound and NEG2 is an auxiliary.  <span style=\"font-weight:bold\"><span style=\"color:blue\">This analysis is still under construction.</span></span></p>"; 
        } else if (document.forms["choices_form"]["neg2-type"][2].checked){
          var d2 = document.getElementById("neg2-fd");
          d2.innerHTML = "<p>NEG1 is bound and NEG2 is an adverb.  <span style=\"font-weight:bold\">This analysis is still under construction.</span></p>"; 
        }
        break;
      case 'fh':
        document.forms["choices_form"]["neg2-type"][0].disabled=false;
        document.forms["choices_form"]["neg2-type"][1].disabled=true;
        document.forms["choices_form"]["neg2-type"][2].disabled=false;

        // fh analysis require the neg-aux choice to be on
        document.forms["choices_form"]["neg-aux"].checked = true;

        d = document.getElementById('neg'+n+'-fh');
        // if neg2 is bound, and we're switching away from neg1 bound,
        // then we need to remove the special message about circumfixes
        // we'll put some advice about this construction there instead
        
        if (document.forms["choices_form"]["neg2-type"][0].checked){
          var d2 = document.getElementById("neg2-b");
          d2.innerHTML = "<p>NEG1 is an auxiliary and NEG2 is bound. Saving this page will create the negative auxiliary for you, as well as the FORM value negform (which your negative auxiliary should select for).  Indicate the bound negator using the customization feature NEGATION plus on the morphology page.</p>";
        } else if (document.forms["choices_form"]["neg2-type"][1].checked){
          var d2 = document.getElementById("neg2-fh");
          d2.innerHTML = "<p>We don't have any analysis for a negation type with two auxiliary verbs.  Please contact matrix-dev about this language.</p>"; 
        } else if (document.forms["choices_form"]["neg2-type"][2].checked){
          var d2 = document.getElementById("neg2-fd");
          d2.innerHTML = "<p>NEG1 is an auxiliary and NEG2 is an adverb.  <span style=\"font-weight:bold\">This analysis is still under construction.</span></p>"; 
        }
        break;
      case 'fd':
        document.forms["choices_form"]["neg2-type"][0].disabled=false;
        document.forms["choices_form"]["neg2-type"][1].disabled=false;
        document.forms["choices_form"]["neg2-type"][2].disabled=false;
        d = document.getElementById('neg'+n+'-fd');
        // if neg2 is bound, we'll put a special message about the status
        // of this type
        if (document.forms["choices_form"]["neg2-type"][0].checked){
          var d2 = document.getElementById("neg2-b");
          d2.innerHTML = "<p>NEG1 is an adverb and NEG2 is bound.  <span style=\"font-weight:bold\">This analysis is still under construction.</span></p>"
        } else if (document.forms["choices_form"]["neg2-type"][1].checked){
          var d2 = document.getElementById("neg2-fh");
          d2.innerHTML = "<p>NEG1 is an adverb and NEG2 is an auxiliary.  <span style=\"font-weight:bold\">This analysis is still under construction.</span></p>"; 
        } else if (document.forms["choices_form"]["neg2-type"][2].checked){
          var d2 = document.getElementById("neg2-fd");
          d2.innerHTML = "<p>NEG1 and NEG2 are adverbs.  <span style=\"font-weight:bold\">This analysis is still under construction.</span></p>"; 
        }
        break;
      default:
        d = null; 
    }
  } else if (n=='2') {
    switch (o) {
      case 'b':
        if (document.forms["choices_form"]["neg1-type"][0].checked) {
          // neg2 is bound, if neg1 is also bound, we tell them to go
          // make a circumfix on the lexicon page.
          d = document.getElementById("neg2-b");
          var chkd = document.forms["choices_form"]["neg1b-neg2b"].checked; 
          var ibox;
          if (chkd) {
            ibox = "<input type=\"checkbox\" name=\"neg1b-neg2b\" checked=\""+chkd+"\">NEG1 bound to Aux requires NEG2 bound to Lexical Verb</input>";
          } else {
            ibox = "<input type=\"checkbox\" name=\"neg1b-neg2b\">NEG1 bound to Aux requires NEG2 bound to Lexical Verb</input>";
          }
           d.innerHTML = "<p>If both NEG1 and NEG2 are bound to the same root, you can set up the dendency using the morphotactics system on the lexicon page. Only specify that one of your LRIs is 'negation plus' (you only need one <em>neg_rel</em>), and set up a requires relation between the two morphemes.</p><p>If NEG1 is bound to an auxiliary and NEG2 is bound to a lexical verb, check the box below.  This will enable several options for you as you define your lexical rules for NEG1 and NEG2 on the morphotactics page.  A value for FORM 'negform' will be added to your grammar (as a subtype of nonfinite).  Indicate the lexical rule corresponding to NEG1 by setting [NEGATION +] (this adds the negative semantics) and the requirement that the complement be [FORM negform].  Likewise, indicate NEG2 by selecting [NEG2 +].  This will set up the NEG2 lexical rule to change the FORM value on its head to negform.  In this way, auxiliary verbs inflected by NEG1 will require their complement to be headed by a verb which has been inflected by NEG2.</p>"+ibox; 
        } else if (document.forms["choices_form"]["neg1-type"][1].checked) {
          // neg2 is bound, if neg1 is a free head, give a message
          d = document.getElementById("neg2-b");
          d.innerHTML = "<p>NEG1 is an auxiliary and NEG2 is bound. Saving this page will create the negative auxiliary for you, as well as the FORM value negform (which your negative auxiliary should select for).  Indicate the bound negator using the customization feature NEGATION plus on the morphology page.</p>";
        } else if (document.forms["choices_form"]["neg1-type"][2].checked) {
          // neg2 is bound, neg1 is an adverb, give a message 
          d = document.getElementById("neg2-b");
          d.innerHTML = "<p>NEG1 is an adverb and NEG2 is bound.  <span style=\"font-weight:bold\">This analysis is still under construction.</span></p>"; 
        }
        break;
      case 'fh':
        // set neg-aux on
        document.forms["choices_form"]["neg-aux"].checked = true;
        if (document.forms["choices_form"]["neg1-type"][0].checked) {
          // neg2 is an aux, neg1 is bound, give an appropriate message
          d = document.getElementById("neg2-fh");
          d.innerHTML = "<p>NEG1 is bound.  NEG2 is an auxiliary.  <span style=\"font-weight:bold\">This analysis is still under construction.</span></p>"; 
        } else if (document.forms["choices_form"]["neg1-type"][1].checked) {
          // neg2 is an aux, neg1 is also an aux, give a message
          d = document.getElementById("neg2-fh");
          d.innerHTML = "<p>We don't have any analysis for a negation type with two auxiliary verbs.  Please contact matrix-dev about this language.</p>"; 
        } else if (document.forms["choices_form"]["neg1-type"][2].checked) {
          // neg2 is an aux, neg1 is an adverb, give a message 
          d = document.getElementById("neg2-b");
          d.innerHTML = "<p>NEG1 is a adverb and NEG2 is an auxiliary.  <span style=\"font-weight:bold\">This analysis is still under construction.</span></p>"; 
        }
        break;
      case 'fd':
        if (document.forms["choices_form"]["neg1-type"][0].checked) {
          // neg2 is an adverb, neg1 is bound, give an appropriate message
          d = document.getElementById("neg2-fd");
          d.innerHTML = '<p>NEG1 is bound.  NEG2 is an adverb.  Select the properties of NEG2 below.  A customization system feature \'requires neg adverb\' has been created for your use on the lexicon page.  Specify NEG1 as \'negation plus\' and \'requires neg adverb plus\'.</p>  NEG2 modifies:  <input type="radio" value="s" name="neg2-mod">  S  <input type="radio" value="vp" name="neg2-mod">  VP  <input type="radio" value="v" name="neg2-mod">  V  <br>  NEG2 is ordered:  <input type="radio" value="before" name="neg2-order">  before  <input type="radio" value="after" name="neg2-order">  after  <input type="radio" value="either" name="neg2-order">  on either side of the category it modifies.  <br>  NEG2 is spelled:  <input type="text" size="20" name="neg2-adv-orth">  <p></p>'; 
        } else if (document.forms["choices_form"]["neg1-type"][1].checked) {
          // neg2 is an adverb, neg1 is an aux, give a message
          d = document.getElementById("neg2-fd");
          d.innerHTML = "<p>NEG1 is and auxiliary, NEG2 is an adverb.  <span style=\"font-weight:bold\">This analysis is still under construction.</span></p>"; 
        } else if (document.forms["choices_form"]["neg1-type"][2].checked) {
          // neg2 is an adverb, neg1 is an adverb, give a message 
          d = document.getElementById("neg2-fd");
          d.innerHTML = "<p>NEG1 and NEG2 are both adverbs.  <span style=\"font-weight:bold\">This analysis is still under construction.</span></p>"; 
        }
        break;
      default:
        d = null;
    }
  }
  if (d != null)
  {
    d.style.display ='block';
  }*/
}

function display_neg_form()
{
  // this function constrols the logical constraints on the form
  // choices.  it's a little like a pre-validation step.
  // there are a lot of possible combinations of choices on the
  // negation page, but most of the combinations won't lead to successful 
  // grammars.  thus the need to prevent users from going down 
  // dead ends we know about. 

  // here we display only the exponence section the user has
  // asked for
  var neg_exp = document.forms["choices_form"]["neg-exp"];
  for (var i=0; i<neg_exp.length;i++)
  {
    if(neg_exp[i].checked){
      set_negexp(neg_exp[i].value); 
    }
  }

  // for simple negation, the selected complements analysis has
  // some dead ends we know about
  if (neg_exp[1].checked) { 
    neg_comp();
  }

  // now we see if there are any choices set for 'negN-type'
  // these are the neg1 and neg2 choices section on the bipartite
  // page.

  if (neg_exp[2].checked) {
    var ntype1 = document.forms["choices_form"]["neg"+1+"-type"]; 
    var ntype2 = document.forms["choices_form"]["neg"+2+"-type"]; 
    var t1,t2;
    for (var j=0;j<ntype1.length;j++){
      if(ntype1[j].checked){
        var v = ntype1[j].value;
        t1 = v;
      }
      if(ntype2[j].checked){
        var v = ntype2[j].value;
        t2 = v;
      }
    }
  }
  set_negmorph(t1,t2);
}

function neg_comp() {
// some restrictions on neg_comps analysis
  // for simple negation, the selected complements analysis has
  // some dead ends we know about
  var comp_neg = document.forms["choices_form"]["comp-neg"];
  var neg_exp  = document.forms["choices_form"]["neg-exp"];
  if (neg_exp[1].checked && comp_neg.checked){
    var comp_neg_head = document.forms["choices_form"]["comp-neg-head"];
    var after = document.forms["choices_form"]["comp-neg-order"][1];
    if(comp_neg_head[1].checked){
      // if the user selects any verb as the head, we can't do the post-comps order
      after.disabled = true;
      after.checked = false;
    } else {
      // turn it back on otherwise
      after.disabled = false;
    }
  }
}
