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

  //n.style.overflow = 'hidden';

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
function do_clone_region(id, iter_var, bAnim)
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

  a.parentNode.insertBefore(n, a);

  multi_init();

  if (bAnim) {
    expand_region(n.id);
  }
}

// clone_region()
// Clone a region and expand using animation
function clone_region(id, iter_var)
{
  do_clone_region(id, iter_var, true);
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
function remove_element(id)
{
  var e = document.getElementById(id);
  e.parentNode.removeChild(e);
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

// Fill a SELECT tag with temp OPTIONs defined by the passed-in arrays
function insert_temp_options(select, values, texts)
{
  for (var i = 0; i < values.length; i++) {
    var o = document.createElement('option');
    o.className = 'temp';
    o.value = values[i];
    o.innerHTML = texts[i];

    select.appendChild(o);
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


//regex_collect
// Pass through the form fields in the page, looking for ones whose
// name attribute matches the pattern.  When one is found, use its
// contents to create an option.
function regex_collect(name, pattern, nameOnly){

  var values = new Array();
  var texts = new Array();

  var e = document.forms[0].elements;
  for (var i = 0; i < e.length; i++) {

    if (e[i].name.search(pattern) != -1) {
      var vn = e[i].name.replace(/_[^_]*$/, '');
      
      var fn = vn;
      var f = document.getElementsByName(vn + '_name');

      if (f && f[0] && f[0].value) {
        if (nameOnly) {
	  vn = fn = f[0].value;
	} 
	else {
	  fn = f[0].value + ' (' + fn + ')';
	}
      }
 
      var len = values.length;
      values[len] = vn;
      texts[len] = fn;
     
    }
  }
  return [values, texts];
}

// fill_regex()
// Fill a SELECT tag with OPTIONs created from the values of any
// form fields on the page whose NAME matches the pattern.  If the
// nameOnly flag is true, make the OPTION's VALUE attribute equal to
// its contents.
function fill_regex(select_name, pattern, nameOnly)
{
  var select = document.getElementsByName(select_name)[0];
  var old_val = select.value;  // store the previously selected option
  var old_text = old_val;
  if (select.selectedIndex != -1) {
    old_text = select.options[select.selectedIndex].innerHTML;
  }

  remove_temp_options(select);

  pattern = '^' + pattern + '$';

  var vn_fn = regex_collect(select_name, pattern, nameOnly);
  var values = vn_fn[0];
  var texts = vn_fn[1];
   
  insert_temp_options(select, values, texts);

  set_select_value(select, old_val, old_text);
  force_layout(select.parentNode);
}

// fill_feature_names()
// Fill a SELECT tag with OPTIONs created from the array features[],
// where every OPTION is a feature name. 
// The cat(egory) argument allows you to restrict the features by category
function fill_feature_names(select_name, cat)
{
  var select = document.getElementsByName(select_name)[0];
  var old_val = select.value;  // store the previously selected option
  var old_text = old_val;
  if (select.selectedIndex != -1) {
    old_text = select.options[select.selectedIndex].innerHTML;
  }

  remove_temp_options(select);

  for (var i = 0; i < features.length; i++) {
    var f = features[i].split(':');
    
    var o = document.createElement('option');
    o.className = 'temp';
    
    var inlist = 'yes';
    //note that this assumes that cat has only a single value when defined.
    if (cat != undefined){
      if (f[2] != cat) {
        inlist = 'no';
      }
    }
	
    if (inlist == 'yes') {
	o.value = f[0];
	o.innerHTML = f[0];
	select.appendChild(o);
    }
  }

  set_select_value(select, old_val, old_text);
  force_layout(select.parentNode);
}


// fill_feature_values()
// Fill a SELECT tag with OPTIONs created from the array features[],
// where every OPTION is a feature value for the feature named
// by the form element named other_name.
function fill_feature_values(select_name, other_name, literal_feature)
{
  var select = document.getElementsByName(select_name)[0];
  var old_val = select.value;  // store the previously selected option
  var old_text = old_val;
  if (select.selectedIndex != -1) {
    old_text = select.options[select.selectedIndex].innerHTML;
  }

  remove_temp_options(select);

  if (literal_feature == 1) {
    var other_val = other_name;
  }  
  else {
    var other_val = document.getElementsByName(other_name)[0].value;
  }

  for (var i = 0; i < features.length; i++) {
    var v = features[i].split(':');

    if (v[0] == other_val) {
      v = v[1].split(';');

      for (var j = 0; j < v.length; j++) {
        var n = v[j].split('|');
        var o = document.createElement('option');
        o.className = 'temp';
        o.value = n[0];
        o.innerHTML = n[1];

        select.appendChild(o);
      }
    }
  }

  set_select_value(select, old_val, old_text);
  force_layout(select.parentNode);
}

// fill_case_patterns()
// Fill a SELECT tag with OPTIONs created from either array
// morph_case_patterns or verb_case_patterns, as determined by the
// morph argument.
function fill_case_patterns(select_name, morph)
{
  var select = document.getElementsByName(select_name)[0];
  var old_val = select.value;  // store the previously selected option
  var old_text = old_val;
  if (select.selectedIndex != -1) {
    old_text = select.options[select.selectedIndex].innerHTML;
  }

  remove_temp_options(select);

  var pats;
  if (morph) {
    pats = morph_case_patterns;
  } else {
    pats = verb_case_patterns;
  }

  for (var i = 0; i < pats.length; i++) {
    var p = pats[i].split(':');
    
    var o = document.createElement('option');
    o.className = 'temp';
    o.value = p[0];
    o.innerHTML = p[1];

    select.appendChild(o);
  }

  set_select_value(select, old_val, old_text);
  force_layout(select.parentNode);
}

// fill_numbers()
// Fill a SELECT tag with OPTIONs created from the array numbers[],
// where every OPTION is a value of the number feature.
function fill_numbers(select_name)
{
  var select = document.getElementsByName(select_name)[0];
  var old_val = select.value;  // store the previously selected option
  var old_text = old_val;
  if (select.selectedIndex != -1) {
    old_text = select.options[select.selectedIndex].innerHTML;
  }

  remove_temp_options(select);

  for (var i = 0; i < numbers.length; i++) {
    var n = numbers[i].split(':');
    var o = document.createElement('option');
    o.className = 'temp';
    o.value = n[0];
    o.innerHTML = n[0];

    select.appendChild(o);
  }

  set_select_value(select, old_val, old_text);
  force_layout(select.parentNode);
}
  
// fill_types()
// Fill a SELECT tag with OPTIONs created from the array types[],
// where every OPTION is a type name.
function fill_types(select_name, type_cat, nameOnly)
{
  var select = document.getElementsByName(select_name)[0];
  var old_val = select.value;  // store the previously selected option
  var old_text = old_val;
  if (select.selectedIndex != -1) {
    old_text = select.options[select.selectedIndex].innerHTML;
  }

  remove_temp_options(select);
 
  var lex_ext = '-' + type_cat + '-lex';

  //collect options from the type fields on the questionnaire
  var pattern = '^'  + '(' + type_cat + ')' + '[0-9]+_name' + '$';

  //FIX not working - output is "undefined" twice or last one on page twice
  //something is wrong with write to or write from array??
  var vn_fn = regex_collect(select_name, pattern, nameOnly);
  values = new Array;
  texts = new Array;
  for (var i = 0; i < vn_fn.length; i++) {
      values[i] = vn_fn[i][0] + lex_ext;
      texts[i] = vn_fn[i][1]+ lex_ext;
  }

  // Collect options from the types() array in choices
  // Note that this assumes a strict naming convention for 
  // lexical types within the customization and no hyphens
  // in the user defined names.
  for (var i = 0; i < types.length; i++) {
      var t = types[i].split(':');

      if (t[1] == type_cat) { 
	  if (texts.indexOf(t[0]) == -1) { //if type is not in the select list already
	      values[values.length] = t[0];
	      texts[texts.length] = t[0];
	  }
      }
  }

  insert_temp_options(select, values, texts);

  set_select_value(select, old_val, old_text);
  force_layout(select.parentNode);
}


//////////////////////////////////////////////////////////////////////
// Multi-SELECT functions
//
// To implement multi-select dropdown controls, we make the associated
// SELECT controls display:none, then insert immediately after them a
// text control and a button that simulate a dropdown.  When either is
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
  span.style.left = 0;
  span.style.top = 0;
  span.style.width = document.body.scrollWidth;
  span.style.height = document.body.scrollHeight;
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
