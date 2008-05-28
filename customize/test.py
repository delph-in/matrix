### $Id: test.py,v 1.12 2008-05-28 21:08:12 sfd Exp $

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

baz = tdl.TDLfile('baz')

baz.add('qqq := [ H.S.L.C.H noun ].')
baz.add('qqq := [ H.S.L.C.H.A + ].')

foo.dump()
bar.dump()
baz.dump()

# Test the list code

t1 = tdl.TDLfile('')
t1.add('t := < a , b, c >.', '')
t1.dump()

t2 = tdl.TDLfile('')
t2.add('t := < a . b >.', '')
t2.dump()

t3 = tdl.TDLfile('')
t3.add('t := < >.', '')
t3.dump()

t4 = tdl.TDLfile('')
t4.add('t := [ FIRST a ].')
t4.add('t := [ REST [ FIRST b, REST null ] ].')
t4.dump()

t5 = tdl.TDLfile('')
t5.add('t := < a, b, c >.')
t5.add('t := [ FIRST A ].')
t5.add('t := [ REST [ FIRST B ] ].')
t5.add('t := [ REST [ REST [ FIRST C ] ] ].')
t5.dump()

t6 = tdl.TDLfile('')
t6.add('t := < a, b, c >.')
t6.add('t := [ REST [ OTHER A ] ].')
t6.dump()

t7 = tdl.TDLfile('')
t7.add('t := < a, b, ... >.')
t7.dump()

t8 = tdl.TDLfile('')
t8.add('t := [ SYNSEM.LOCAL.CAT.HEAD verb ].')
t8.add('t := [ SYNSEM.LOCAL.CAT.HEAD noun ].')
t8.add('t := [ SYNSEM.LOCAL.CAT other ].')
t8.add('t := one-type.')
t8.add('t := two-type.')
t8.add('t := #one.')
t8.dump()

t9 = tdl.TDLfile('')
t9.add('t := [ SYNSEM [ LOCAL [ CAT [ HEAD verb ] ] ] ].')
t9.dump()

t10 = tdl.TDLfile('')
t10.add('t := type & [ ARG-ST < [ ], [ ] > ].')
t10.add('t := [ ARG-ST < [ HEAD noun ], [ ] > ].')
t10.dump()
