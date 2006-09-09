#!/usr/local/bin/python

import tdl

foo = tdl.TDLfile('foo')

foo.add('shp := [ H.S.L.C.V.C < n, v > ].', '1st\ncomment')
foo.add('shp := [ H.S.L.C.V.C < n, n, v> ].', '2nd\ncomment')
foo.add('shp := bhsp & hf & [ H.S.L.C.H noun ].', '3rd\ncomment')
foo.add('phs := bhsp & hf & [ H.S.L.C.H noun ].', '4th\ncomment')

bar = tdl.TDLfile('bar')

bar.add('sss := [ H.S.L.C.V.C < n, v > ].', '5th\ncomment')
bar.add('hhh := [ H.S.L.C.V.C < n, n, v> ].', '6th\ncomment')
bar.add('ppp := [ H.S.L.C.V.C < n, n, v> ].', '7th\ncomment')

foo.save()
bar.save()
