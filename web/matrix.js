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

// TJT 2014-08-26
// toggle_element(id, how="toggle", switchOn=null)
// Toggle display none/block on id
//  id (string): which id to toggle
//  how (string): toggle, turn on, or turn off div with id
//  switchOn ([string, ...]): list of options able to activate toggle in select
function toggle_element(id, how, switchOn) {
  how = how || "toggle"; // set default to "toggle"
  switchOn = switchOn || null; // set default to null
  switchActive = true; // Ignore switch unless switchOn activates
  // Get element to toggle
  element = document.getElementById(id);
  if (switchOn) {
    switchActive = false; // If switchOn, do nothing by default
    // Check to see if the node is a standard node
    if (element != null && element.nodeType == 1) {
      // This switch currently only works with select tags
      if (this.tagName.toLowerCase() == "select") {
	// If switchOn matches selected element, then do the toggling
	if (switchOn.indexOf(this.options[this.selectedIndex].value) > -1) {
	  switchActive = true;
	}
      }
    }
  }
  // If element found and switch active, do the toggling
  if (element != null && switchActive) {
    if (how == "toggle") {
      element.style.display = (element.style.display != 'block') ? 'block' : 'none';
    }
    else if (how == "on") {
      element.style.display = 'block';
    }
    else if (how == "off") {
      element.style.display = 'none';
    }
  } 
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

// _display()
// Display or hide an entity
function _display(_id, display_or_not)
{
 	var e = document.getElementById(_id);
	if (e != null)
	{
		if (display_or_not == 'no' || display_or_not == 'n' || display_or_not == 'none')
			e.style.display = 'none';
		else
			e.style.display = 'block';
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
    if (elms[i].name == "subpage" || elms[i].name=="delivery" || elms[i].name=="customize") {
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
    if (elms[i].name == "subpage" || elms[i].name=="delivery" || elms[i].name=="customize") {
      form.removeChild(elms[i]);
    }
  }
  // Create link to new page
  var inp = document.createElement('input');
  inp.type= "hidden";
  inp.name= "subpage";
  inp.value=subpage
  form.appendChild(inp);
  form.submit();
  // TJT 2014-08-21: Set url to new page
  //var newPage = "matrix.cgi?subpage=" + subpage;
  //window.location.href = newPage;
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
  
  // Show/hide options based on previous choices
  //if (check) {
  //  toShow = d.getElementById(check);
  //  if (toShow) {
  //    toShow.style.display = "block";
  //  }
  //}

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
  //original
  do_clone_region(id, iter_var, true, bShow);
  //if (check) {
  //  do_clone_region(id, iter_var, check, true, bShow);
  //}
  //else {
  //  do_clone_region(id, iter_var, false, true, bShow);
  //}
}

// clone_region()
// Clone a region and expand *without* animation
function clone_region_noanim(id, iter_var)
//function clone_region_noanim(id, iter_var, check)
{
  //original
  do_clone_region(id, iter_var, false);
  //if (check) {
  //  do_clone_region(id, iter_var, check, false);
  //}
  //else {
  //  do_clone_region(id, iter_var, false, false);
  //}
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

// remove_element_all()
// Remove all elements starting with id
// and remove any associated show/hide buttons
// and remove any show/hide errors
function remove_element_all(id, suffix)
{
  for(var i = 0; i < 100; i++)
  {
    var child = id + '_' + suffix + i;
	var e = document.getElementById(child);
    if (e != null) remove_element(child);
  }
}

// check_radio_button(_name, _type)
// TJT 2014-09-02:
// Type can be left out, "yes", "no", "last", or a number
// If left out, _type will default to "yes"
function check_radio_button(name, mode, switchOn) {
        
	mode = mode || 'yes';
	switchOn = switchOn || null;
	switchActive = true;
	var radio = document.getElementsByName(name);
	
	// TJT 2014-09-03: Adding switch to only execute on particular selection
	if (switchOn) {
	  switchActive = false; // If switchOn, do nothing by default
	  // Check to see if the node is a standard node
	  if (element != null && element.nodeType == 1) {
	    // This switch currently only works with select tags
	    if (this.tagName.toLowerCase() == "select") {
	      // If switchOn matches selected element, then do the toggling
	      if (switchOn.indexOf(this.options[this.selectedIndex].value) > -1) {
		switchActive = true;
	      }
	    }
	  }
	}
	
	if (switchActive) {
	  if(mode == 'no') {
	      radio[0].checked = true;
	      //radio[1].checked = false;
	    }
	  // TJT 2014-09-03: Check a radio button by number
	  else if (typeof mode == 'number') {
	    if (mode < radio.length) {
	      radio[mode].checked = true;
	    }
	  }
	  // TJT 2014-09-03: Check the last radio button
	  else if (mode == 'last') {
	    radio[radio.length-1].checked = true;
	  }
	  else {
	    //radio[0].checked = false;
	    radio[1].checked = true;
	  }
	}
}

// uncheck_all_radio_button(_name)
function uncheck_all(_name)
{
	var _boxes = document.getElementsByName(_name);
	var i = 0;
	while(1)
	{
		if(_boxes[i] == null)
			break;
		_boxes[i].checked = false;
		i++;
	}
}

//empty_value(_name, _i)
function empty_value(_name, _i)
{
	var e = document.getElementsByName(_name);
	e[_i].value = '';
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

// fill_feature_names_only_customized(cat)
// This is used on the other features.
// Only feature(s) that users specify on the customization system can
// show up as an existing value type with bool and luk.
function fill_feature_names_only_customized(cat)
{
  var items = new Array()
  for (var i = 0; i < features.length; i++) {
    var f = features[i].split(':');
    if (f[3] == 'n' || f[3] == 'no' || f[3] == '')
      continue;
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
  // add clausal complement strategies as possible argument structure
  return items
}

// fill_numbers()
// Return items from the array numbers[], where every OPTION is a value of the
// number feature.
function fill_numbers()
{
  var items = new Array();
  for (var i = 0; i < numbers.length; i++) {
    var n = numbers[i].split(':');
    items.push([n[0],n[0]]);
  }
  return items
}


function fill_forms()
{
  var items = new Array();
  for (var i = 0; i < forms.length; i++) {
    var n = forms[i].split(':');
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

  // Create the text box to show choices while not open
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

  // Create button to prompt users to open multi_select
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
      
      // TJT 2014-09-05: Making text node into label for checkbox so that clicking on
      // text makes the checkbox checked
      // output: <label><input type="checkbox" id="id">text</label>
      var label = document.createElement("label");
      label.setAttribute("class","checkLabel");
      var check = document.createElement('input');
      check.type = 'checkbox';
      check.id = select_name + '__' + ops[i].value;
      check.onkeypress = function(e) { return multi_keypress(e, select.name); }
      if (ops[i].disabled) { // TJT 2014-09-05: Get disabled
	check.disabled = true;
	label.style.color = "#888"; // TJT 2014-09-05: Grey out disabled text
      }
      check.onkeypress = function(e) { return multi_keypress(e, select.name); }
      label.appendChild(check); // TJT 2014-09-05: Add check to label, label to span
      label.innerHTML = label.innerHTML + " " + ops[i].innerHTML;
      span.appendChild(label);
      if (indexOf(vals, ops[i].value) != -1) {
        label.firstChild.checked = true;  // Must be done AFTER insertion on IE
      }
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
  for (var label = box.firstChild; label; label = label.nextSibling) {
    // TJT 2014-09-05: Unwrap "<label>"
    if (label.tagName == 'LABEL') {
      check = label.firstChild;
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

// subpair_matrix_fill_pred is similar to fill pred, except that it uses the matrixorth
// and matrixpred values. These values distinguish between the subordinator morphemes
// in the matrix and subordinate clauses in claual modifiers with subordinate pairs
function subpair_matrix_fill_pred(name,pos)
{
  var elms = document.getElementsByName(name+'_matrixorth');
  var word = '';
  for (var i = 0; i < elms.length; i++) {
    if (elms[i].type == "text") {
      word = elms[i].value;
    }
  }
  var matrixpred = "_"+word+"_"+pos+"_rel";
  elms = document.getElementsByName(name+'_matrixpred');
  for (var i = 0; i < elms.length; i++) {
    if (elms[i].type == "text" && elms[i].value == '' && word != '') {
      elms[i].value = matrixpred;
      var text_elms = document.getElementsByTagName('input');
      var match_inds = [];
      for (var j = 0; j < text_elms.length; j++) {
        if (text_elms[j].type == "text" && text_elms[j].value.match(new RegExp("^_"+word+"_"+pos+"_?[0-9]*_rel$",""))){
          match_inds.push(j);
        }  
      }
      if (match_inds.length > 1){
        for (var j = 0; j < match_inds.length; j++) {
          text_elms[match_inds[j]].value = matrixpred.replace("_rel", "_"+(j+1)+"_rel");
        }
      }
    }
  }
}

// subpair_matrix_fill_pred is similar to fill pred, except that it uses the matrixorth
// and subord orth values to fill the and subordpred value in the form 
// _subordorth+matrixorth_subord_rel (eg. _if+then_subord_rel). 
function subpair_subord_fill_pred(name,pos)
{
  var elms = document.getElementsByName(name+'_matrixorth');
  var matrixword = '';
  for (var i = 0; i < elms.length; i++) {
    if (elms[i].type == "text") {
      matrixword = elms[i].value;
    }
  }
  var elms = document.getElementsByName(name+'_subordorth');
  var subordword = '';
  for (var i = 0; i < elms.length; i++) {
    if (elms[i].type == "text") {
      subordword = elms[i].value;
    }
  }
  var subordpred = "_"+subordword+"+"+matrixword+"_"+pos+"_rel";
  elms = document.getElementsByName(name+'_subordpred');
  for (var i = 0; i < elms.length; i++) {
    if (elms[i].type == "text" && elms[i].value == '' && subordword != '') {
      elms[i].value = subordpred;
      var text_elms = document.getElementsByTagName('input');
      var match_inds = [];
      for (var j = 0; j < text_elms.length; j++) {
        if (text_elms[j].type == "text" && text_elms[j].value.match(new RegExp("^_"+subordword+"+"+matrixword+"_"+pos+"_?[0-9]*_rel$",""))){
          match_inds.push(j);
        }  
      }
      if (match_inds.length > 1){
        for (var j = 0; j < match_inds.length; j++) {
          text_elms[match_inds[j]].value = subordpred.replace("_rel", "_"+(j+1)+"_rel");
        }
      }
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
// Special functions for Subpages
////////////////////////////////////////////////////////////

////////
// Other Features Subpage
////////


// set_form_feature automatically hide and show section based on radio choice
//
function set_form_feature()
{
  var divs = document.getElementsByClassName("form_switch");
	for(var i=0; i<divs.length;i++){
    var d = divs[i];
    d.style.display = 'none';
	}
  var d = document.getElementById('form');
  d.style.display ='block';
}

////////
// Sentential Negation Subpage
////////

// set_negexp(n) automatically hide and show section based on radio choice
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
//////////////////////////////////////////////////////////
// For adnominal possession subpage                     //
//////////////////////////////////////////////////////////
function set_possessum(n)
{
  var value = n;
  var divs = document.getElementsByClassName("possessum_affix_switch");
	for(var i=0; i<divs.length;i++){
    var d = divs[i];
    d.style.display = 'none';
	}
  var d;
  switch (n){
    case '0':
      var d = document.getElementById('agree');
      break;
    case '1':
      var d = document.getElementById('non-agree');
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
  } else { t = 'default'; }

  // if t != 'default', then we
  //  have a well defined bipartite negation
  //  type to implement for the user.
  //  set the choice accordingly
  //  this choice is useful at customize time
  //
  //  also, set subchoices to interface with
  //  deffile side-effects (neg-aux), and 
  //  neg library dependencies
  if (t.search(/head/) > -1) {
    document.forms['choices_form']['neg-aux'].checked= true; 
  } else {
    document.forms['choices_form']['neg-aux'].checked = false;
  }
  if (t.search(/comp|mod/) > -1) {
    document.forms['choices_form']['adv-neg'].checked= true; 
  } else {
    document.forms['choices_form']['adv-neg'].checked = false;
  }
  if (t.search(/infl/) > -1) {
    document.forms['choices_form']['infl-neg'].checked= true; 
  } else {
    document.forms['choices_form']['infl-neg'].checked = false;
  }


  var d = document.getElementById('bineg_fb');
  if (t != 'default') {
    document.forms['choices_form']['bineg-type'].value = t;
    // now set up the page accordingly
    d.innerHTML=document.getElementById(t+'-neg').innerHTML;
  } else {
    d.innerHTML='<p>Please choose a morpheme type for each exponent.</p>';
  }
}

function display_form_choice()
{
    set_form_feature('2')
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
    set_negmorph(t1,t2);
  }
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

var scaled = 0;
window.onresize=scalenav;

function scalenav() {
  // make smaller nav is below threshold
  var d = document.getElementById("navmenu");
  var map = { "General Information":"Gen Info",
              "Tense, Aspect and Mood":"TAM",
              "Direct-inverse":"Dir-inv",
              "Sentential Negation":"Neg",
              "Matrix Yes/No Questions":"Y/N Qs",
              "Information Structure": "Info Str",
              "Argument Optionality":"Arg Opt",
              "Toolbox Import":"Tb Import",
              "Test Sentences":"Test S",
              "Morphology":"Morph",
              "Other Features":"Features",
              "Coordination":"Coord", 
	      "Adnominal possession" : "Poss"};
  if (window.scaled==0 && d.clientWidth<150) {
    window.scaled=1;
    var list = document.getElementsByClassName("navlinks");
    for(var i=0;i<list.length;i++){
      for(b in map){
        if(list[i].innerHTML==b){
          list[i].innerHTML=map[b];
        }
      }
    }
  } else if (scaled==1 && d.clientWidth>150){
    window.scaled=0;
    var list = document.getElementsByClassName("navlinks");
    for(var i=0;i<list.length;i++){
      for(b in map){
        if(list[i].innerHTML==map[b]){
          list[i].innerHTML=b;
        }
      }
    }
  }
}

// call customize grammar from a subpage
function nav_customize(type) {
  var f = document.forms['choices_form']; 
  var elms = document.getElementsByTagName('input');
  for (var i = 0; i < elms.length; i++) {
    if (elms[i].name=="subpage" || elms[i].name=="delivery" || elms[i].name=="customize") {
      f.removeChild(elms[i]);
    }
  }

  var t = document.createElement('input');
  var i = document.createElement('input');

  t.id="delivery";

  t.type= "hidden";
  i.type= "hidden";

  t.name= "delivery";
  i.name= "customize";

  t.value=type;
  i.value="customize"

  f.appendChild(t);
  f.appendChild(i);
  f.submit();

  f.removeChild(t);
  f.removeChild(i);
}

