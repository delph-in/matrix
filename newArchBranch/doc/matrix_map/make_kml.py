import sys
from collections import defaultdict
from xml.sax.saxutils import escape

print r'''<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
  <Document>
    <name>Matrix Languages</name>
    <open>1</open>
    <description>Languages described by the Matrix Customization System</description>'''

wals = defaultdict(list)
for line in open('languages.tab','r'):
    toks = [escape(x.strip()) for x in line.split('\t')]
    wals[toks[7]] += [dict(zip(['wals code','name','latitude','longitude',
                                'genus','family','subfamily','iso codes'],
                               toks))]
languages = defaultdict(list)
for line in open('matrix_langs','r'):
    toks = [escape(x.strip()) for x in line.split('\t')]
    if int(toks[4]) < 2006: continue
    languages[toks[4]] += [dict(zip(['language name','code','language family',
                                     'developer','year'],
                                    toks))]
# first make styles for each year
colors = ['blue','red','green','ltblue','yellow','purple','orange','pink']
color_idx = 0
for year in sorted(languages.keys()):
    print '''  <Style id="style%s">
    <IconStyle>
      <Icon>
        <href>http://maps.gstatic.com/intl/en_us/mapfiles/ms/micons/%s-dot.png</href>
      </Icon>
    </IconStyle>
  </Style>''' % (year,colors[color_idx])
    color_idx = (color_idx + 1) % len(colors)

for year in sorted(languages.keys()):
    print '''    <Folder>
      <name>Languages %s</name>
      <description>Languages created in %s</description>''' % (year,year)
    for lang in languages[year]:
        if lang['code'] in wals:
            if len(wals[lang['code']]) > 1:
                match_name = [wl for wl in wals[lang['code']]
                              if wl['name'] == lang['language name']]
                if len(match_name) > 0:
                    wals_lang = match_name[0]
                else:
                    wals_lang = wals[lang['code']][0]
            else:
                wals_lang = wals[lang['code']][0]
            lang_data = dict(lang)
            lang_data.update(wals_lang)
            print r'''      <Placemark>
        <name>%(name)s</name>
        <description>%(genus)s %(family)s %(subfamily)s.
          Created by %(developer)s in %(year)s.</description>
        <styleUrl>#style%(year)s</styleUrl>
        <Point>
          <coordinates>%(longitude)s,%(latitude)s,0</coordinates>
        </Point>
        <open>1</open>
      </Placemark>''' % lang_data
        else:
          print >>sys.stderr, "### NO DATA FOR " + lang['code'] + "!!! ###"
    print '    </Folder>'


print r'''  </Document>
</kml>'''
