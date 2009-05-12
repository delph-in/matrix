### $Id: test.py,v 1.13 2008-06-27 20:45:22 sfd Exp $

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
t1.add('t1 := < a , b, c >.', '')
t1.dump()

t2 = tdl.TDLfile('')
t2.add('t2 := < a . b >.', '')
t2.dump()

t3 = tdl.TDLfile('')
t3.add('t3 := < >.', '')
t3.dump()

t4 = tdl.TDLfile('')
t4.add('t4 := [ FIRST a ].')
t4.add('t4 := [ REST [ FIRST b, REST null ] ].')
t4.dump()

t5 = tdl.TDLfile('')
t5.add('t5 := < a, b, c >.')
t5.add('t5 := [ FIRST A ].')
t5.add('t5 := [ REST [ FIRST B ] ].')
t5.add('t5 := [ REST [ REST [ FIRST C ] ] ].')
t5.dump()

t6 = tdl.TDLfile('')
t6.add('t6 := < a, b, c >.')
t6.add('t6 := [ REST [ OTHER A ] ].')
t6.dump()

t7 = tdl.TDLfile('')
t7.add('t7 := < a, b, c >.')
t7.add('t7 := < A, B, ... >.')
t7.dump()

t8 = tdl.TDLfile('')
t8.add('t8 := [ SYNSEM.LOCAL.CAT.HEAD verb ].')
t8.add('t8 := [ SYNSEM.LOCAL.CAT.HEAD noun ].')
t8.add('t8 := [ SYNSEM.LOCAL.CAT other ].')
t8.add('t8 := one-type.')
t8.add('t8 := two-type.')
t8.add('t8 := #one.')
t8.dump()

t9 = tdl.TDLfile('')
t9.add('t9 := [ SYNSEM [ LOCAL [ CAT [ HEAD verb ] ] ] ].')
t9.dump()

t10 = tdl.TDLfile('')
t10.add('t10 := type & [ ARG-ST < [ ], [ ] > ].')
t10.add('t10 := [ ARG-ST < [ HEAD noun ], [ ] > ].')
t10.dump()
