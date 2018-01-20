# from sets import Set
from gmcs.linglib.parameters import determine_vcluster
from gmcs.linglib.lexical_items import update_lex_items_vcluster
from gmcs.linglib.clausalcomps import extraposed_comps

######################################################################
# customize_word_order()
#   Create the type definitions associated with the user's choices
#   about basic word order, including information about adpositions
#   and auxiliaries.

def customize_word_order(mylang, ch, rules):

    wo = ch.get('word-order')

    mylang.set_section('phrases')

    # Add type definitions.

    # Handle major constituent order first.  This function returns the hs and hc
    # values that other parts of this code will want.

    linear_precedence = customize_major_constituent_order(wo, mylang, ch, rules)
    hs = linear_precedence['hs']
    hc = linear_precedence['hc']


    # Head specifier rules

    customize_np_word_order(mylang, ch, rules)

    # ERB 2006-09-14 Then add information as necessary to handle adpositions,
    # free auxiliaries, etc.

    #In general, we might also find word order sensitive to
    #clause type (matrix v. subordinate) and dependent type.

    orders = determine_consistent_order(wo,hc,ch)
    specialize_word_order(hc,orders,mylang,ch,rules)

    customize_subord_word_order(mylang,ch,wo,rules)



def customize_major_constituent_order(wo, mylang, ch, rules):

    # ERB 2006-09-14 The most elegant factoring of just the major
    # constituent information uses the same type names for head-comp and
    # comp-head phrases.  This doesn't scale when we have to worry about
    # adpositions and auxiliaries (and other twists on word order) where
    # we might want both comp-head and head-comp in a single language.
    # To try to keep the elegant factoring, I'm going to store the
    # type names in variables.

    hc = ''
    hs = ''

    # This part of the code handles the following basic word orders:
    # all six strict orders, V-final and V-initial.  These 8 possible
    # orders can be grouped according to head-comp order, head-subj order,
    # and whether or not complements must attach before subjects, or subjects before
    # complements.  I'm treating SVO and OVS as having complements attaching
    # lower.

    # (Are there languages which allow all orders in which S attaches
    # ouside O as basic word orders?  Likewise all cases where O attaches
    # outside S?)

    # ERB 2007-01-21 Moving to message-free universe, and now hypothesizing
    # that all rules which attach subjects attend to clausal semantics.
    # Thus, the head-subj rules that are defined here inherit from
    # decl-head-subj-phrase (imp-head-subj-phrase is also available).

    # ASF 2008-11-03 v2 analysis requires MC feature is not passed up to mother in
    # head - comp and not from mod to mother, putting it back for other wo options

    # KPH 2017-12-14 Removed mc identity between non head daughter and mother in
    # basic-head-mod. This is not the case for clausal modifiers (in whcih the modifer
    # is MC -).

    if not wo == 'v2':
        mylang.add_literal(';Constraint on MC used to be part of matrix.tdl\n;' +
                           ';it applies to all wo implementations, except for v2')
        mylang.add('basic-head-comp-phrase :+\
                [ SYNSEM.LOCAL.CAT.MC #mc,\
                  HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].',
                   section='addenda')
        # mylang.add('basic-head-mod-phrase-simple :+\
        #         [ SYNSEM.LOCAL.CAT.MC #mc, \
        #           NON-HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].',
        #            section='addenda')


    # Head-comp order

    if wo == 'sov' or wo == 'osv' or wo == 'ovs' or wo == 'v-final':
        hc = 'comp-head'
        mylang.add(hc + '-phrase := basic-head-1st-comp-phrase & head-final.')

    if wo == 'svo' or wo == 'vos' or wo == 'vso' or wo == 'v-initial':
        hc = 'head-comp'
        mylang.add(hc + '-phrase := basic-head-1st-comp-phrase & head-initial.')

    # Head-subj order

    if wo == 'osv' or wo == 'sov' or wo == 'svo' or wo == 'v-final':
        hs = 'subj-head'
        mylang.add(hs + '-phrase := decl-head-subj-phrase & head-final.')

    if wo == 'ovs' or wo == 'vos' or wo == 'vso' or wo == 'v-initial':
        hs = 'head-subj'
        mylang.add(hs + '-phrase := decl-head-subj-phrase & head-initial.')


    # Complements attach before subjects
    if wo == 'ovs' or wo == 'vos' or wo == 'sov' or wo == 'svo':
        # If the language is OVS and extraposes clausal complements, we need subjects to attach low:
        if not ((wo == 'ovs' or wo == 'vos') and 'comps' in ch and extraposed_comps(ch)):
            mylang.add(hs + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].')

    # Subjects attach before complements
    # ASF 2008-11-20 in order to allow for aux with vp-comp for VSO and OSV
    # languages, the standard analysis needs to be adapted to a LIGHT +
    # constraint on the hs-rule.
    # OZ 2017-12-21 We also need low subject attachment for OVS languages with extraposed clausal complements.
    auxcomp = ch.get('aux-comp')
    if (wo == 'vso' or wo == 'osv') or (wo == 'ovs'
                                        and 'comps' in ch and extraposed_comps(ch)):
        if ch.get('has-aux') == 'yes' and auxcomp == 'vp':
            mylang.add(hs + '-phrase := [ HEAD-DTR.SYNSEM.LIGHT + ].')
        else:
            mylang.add(hc + '-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].')

        # LLD 2016-03-24 to allow argument optionality with VSO and OSV languages,
        # we have to move COMPS < > from basic-head-opt-subj-phrase in matrix.tdl and
        # only add it with languages where O attaches lower than S. To prevent spurious
        # ambiguity, we add SUBJ < > to head-opt-comp in these languages instead.

    if wo == 'vso' or wo == 'osv':
        mylang.add('basic-head-opt-comp-phrase :+ [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.SUBJ < > ].', section='addenda')
    else:
        mylang.add('basic-head-opt-subj-phrase :+ [ HEAD-DTR.SYNSEM.LOCAL.CAT.VAL.COMPS < > ].', section='addenda')

    # ERB 2006-09-14 Free word order is a big fat special case:

    # Module for free word order, i.e., all possible orders of head, subj,
    # comps (thinking initially about V, S, and O).  Use this with
    # free-order-rules.tdl, which is partially redundant to
    # V-initial-rules.tdl and V-final-rules.tdl.  The difference is that
    # those currently don't have the head-2nd-comp rules.

    # Just using the V-final and V-initial modules together is
    # unsatisfactory, since that system gives multiple parses for V-medial
    # sentences.  On the other hand, the SVO and OVS modules together will
    # generate SOV and VOS words, but not VSO and OSV.  Any other
    # combinations would (I believe) lead to respecifications of types
    # (head-subj-phrase, etc) rather than multiple parallel types.

    # The root of the problem seems to be that we need the subject to be
    # able to attach inside the object(s) for VSO and OSV, but at the same
    # time, we don't want complete flexibility on order of attachment when
    # the verb is in the middle -- that would give spurious ambiguity.

    # This solution adopts the xmod hierarchy to enforce right-first
    # attachment.  That is, all arguments appears to the right of the verb
    # must attach before all arguments appearing to the left.  The
    # linguistic prediction of this analysis is that free word order
    # languages do not have a consistent VP consituent, even when the verb
    # and object are adjacent (OV order).

    # Using a separate feature for tracking argument attachment (as
    # opposed to modifier attachment).  We might be able to collapse these
    # one day, but that's not obvious.

    # ERB 2006-09-14 This looks like a case for :+, even in this code,
    # since we're adding to something defined in matrix.tdl.


    if wo == 'free':
        mylang.add('synsem :+ [ ATTACH xmod ].',
                   'We can\'t just use the V-final and V-initial word\n' +
                   'order modules together to get a good free word order\n' +
                   'module. The root of the problem seems to be that we\n' +
                   'need the subject to be able to attach inside the\n' +
                   'object(s) for VSO and OSV, but at the same time, we\n' +
                   'don\'t want complete flexibility on order of attachment\n' +
                   'when the verb is in the middle -- that would give\n' +
                   'spurious ambiguity.  This solution adopts the xmod\n' +
                   'hierarchy to enforce right-first attachment.  That is,\n' +
                   'all arguments appears to the right of the verb must\n' +
                   'attach before all arguments appearing to the left.  The\n' +
                   'linguistic prediction of this analysis is that free\n' +
                   'word order languages do not have a consistent VP\n' +
                   'consituent, even when the verb and object are adjacent\n' +
                   '(OV order).  Using a separate feature for tracking\n' +
                   'argument attachment (as opposed to modifier\n' +
                   'attachment).  We might be able to collapse these one\n' +
                   'day, but that\'s not obvious.',
                   section='addenda')

        # ASF 2008-11-18, if free wo lgge has aux and aux precedes verb,
        # the enforced attachment must apply in the other direction.

        if ch.get('has-aux') == 'yes' and  ch.get('aux-comp-order') == 'before':
            mylang.add('head-final-head-nexus := head-final & \
                [ SYNSEM.ATTACH lmod,\
                  HEAD-DTR.SYNSEM.ATTACH notmod-or-lmod ].')
            mylang.add('head-initial-head-nexus := head-initial &\
                [ SYNSEM.ATTACH rmod ].')
        else:
            mylang.add('head-initial-head-nexus := head-initial & \
                [ SYNSEM.ATTACH lmod,\
                  HEAD-DTR.SYNSEM.ATTACH notmod-or-lmod ].')

            mylang.add('head-final-head-nexus := head-final &\
                [ SYNSEM.ATTACH rmod ].')

        mylang.add('head-mod-phrase :+\
                [ SYNSEM.ATTACH #attach,\
                  HEAD-DTR.SYNSEM.ATTACH #attach ].',
                   'We\'ll need to add identification of ATTACH between\n\
    mother and head-daughter for all other kinds of phrases\n\
    if we do this.  Just for illustration, I\'m putting it\n\
    in for head-adjunct phrases here:',
                   section='addenda')


    # ASF (2008-11-03) Another big special case: v2
    #
    # This implementation does Autronesian-type v2, but without discontinuous nps.
    # The only word order constraint is that the (verbal) head of the phrase must
    # be in second position.
    # It can be preceded by a noun phrase, verb or verbal cluster.
    # Interaction with auxiliaries is not implemented for now, because it is not
    # clear what may occur (so auxiliaries can occur anywhere for now, as long as
    # the v2 constraint is respected)
    # Also note that the implementation may need to be revised when more complex
    # phenomena (such as clause final verbal cluster and vp-fronting) are
    # implemented
    #

    if wo == 'v2':
        mylang.add('verbal-head-nexus := headed-phrase & \
                [ SYNSEM.LOCAL.CAT.HEAD verb ].')
        mylang.add('head-initial-head-nexus := head-initial & \
                [ SYNSEM.LOCAL.CAT.MC na & #mc, \
                  HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].')
        # OZ 2017-11-13 [ MC bool ] is to allow v2 order in subordinate clauses, like in Wabmbaya.
        if not ch.get('subord-word-order') or ch.get('subord-word-order') == 'same':
            mylang.add('head-final-head-nexus := head-final & \
                  [ SYNSEM.LOCAL.CAT.MC bool, \
                    HEAD-DTR.SYNSEM.LOCAL.CAT.MC na ].')
        # OZ 2017-11-13 For strict subordinate order, like in formal German,
        # need [ MC + ] for the head-final phrase.
        else:
            mylang.add('head-final-head-nexus := head-final & \
            [ SYNSEM.LOCAL.CAT.MC +, \
              HEAD-DTR.SYNSEM.LOCAL.CAT.MC na ].')


        #rules shared among free and v2

    if wo == 'free' or wo == 'v2':
        mylang.add('head-subj-phrase := decl-head-subj-phrase & head-initial-head-nexus.')
        mylang.add('subj-head-phrase := decl-head-subj-phrase & head-final-head-nexus.')
        mylang.add('head-comp-phrase := basic-head-1st-comp-phrase & head-initial-head-nexus.')
        mylang.add('comp-head-phrase := basic-head-1st-comp-phrase & head-final-head-nexus.')
        mylang.add('head-comp-phrase-2 := basic-head-2nd-comp-phrase & head-initial-head-nexus.')
        mylang.add('comp-head-phrase-2 := basic-head-2nd-comp-phrase & head-final-head-nexus.')


    # Add rule definitions for major constituent order.

    if wo == 'free' or wo == 'v2':
        rules.add('head-comp := head-comp-phrase.')
        rules.add('head-subj := head-subj-phrase.')
        rules.add('comp-head := comp-head-phrase.')
        rules.add('subj-head := subj-head-phrase.')
        rules.add('head-comp-2 := head-comp-phrase-2.')
        rules.add('comp-head-2 := comp-head-phrase-2.')
    # Assume at this point that there's a good value of wo.
    # Rule names are stored in hs and hc, since they're the same as type names
    # without the -phrase suffix.
    else:
        rules.add(hc + ' :=  ' + hc + '-phrase.')
        rules.add(hs + ' :=  ' + hs + '-phrase.')

    return {'hs': hs, 'hc': hc}



# ERB 2006-09-15 Subroutine for emitting additional information about
# head-complement and head-subject rules as required for adpositions and
# auxiliaries.

# aux		 adp		head-comp	comp-head	#rules    add to rules.tdl
# ---		 ---		---------	---------	------    ----------------
# ov-auxv 	 ov-prep	v:AUX + | adp	~adp:AUX -	2         head-comp: both
# vo-vaux	 vo-post	~adp:AUX - 	v:AUX + | adp	2         comp-head: both
# free-auxv	 free-prep	unrestricted	~adp:AUX - 	2         --
# free-vaux      free-post	~adp:AUX -	unrestricted	2         --
# free-auxv	 free-post	~adp		AUX -		2         --
# free-vaux      free-prep	AUX -		~adp		2         --
# easy		 ov-prep	adp		~adp		2         head-comp: adp
# easy		 vo-post	~adp		adp		2         comp-head: adp
# easy		 free-prep	unrestricted	~adp		2         --
# easy		 free-post	~adp		unrestricted	2         --
# ov-auxv	 easy		v:AUX +		AUX -		2         head-comp: aux
# vo-vaux	 easy		AUX -		v:AUX +		2         comp-head: aux
#
# Not bothering with the case where adpositions aren't fixed in their order,
# since I don't believe it exists.

# ERB 2006-09-15 Trying to make each tdl snippet appear only once in this code.
# The individual constraints from the table above are:

# v:AUX + | adp    HEAD +vp & [ AUX + ]
# ~adp             HEAD +nvjrcdmo
# adp              HEAD adp
# AUX -            [ AUX - ]
# v:AUX +          HEAD verb & [ AUX + ]

# These can apply to either head-comp or comp-head, depending.

# ERB 2006-10-05 But: We need to worry about question particles now,
# too.  As I add to this and the space gets more complex, I wish that
# there was some what I could take advantage of the head-types hiearchy
# better.

# In particular, the ~adp constraint right now adds +nvjrcdmo.
# But, the same phrase type might need to be compatible with comp,
# so I have to weaken that do +nvjrdmo, and then further constrain it
# to +nvjrcdmo.  Likewise, ~comp boils down to +nvjrdmo, possibly
# further constrained to +nvjrpdmo.  But I can't just add both
# of those constraints, since the tdl won't compile (you can't add
# a type to a conjunction if its supertype or subtype is mentioned).

# My first pass at a solution is to first collect two kinds of information
# about each of head-comp and comp-head:

# head-comp-is is a list which stores positive statements about
# what can be the head of a head-comp phrase.  If the word order is
# free, we shouldn't see any statements here, because anything can be
# a head. (Similarly for comp-head-is.)

# head-comp-is-not is a list which stores negative statements about
# possible heads of head-comp phrase, i.e., what can't be the head.
# We will have interesting values here even in the free word order
# case unless all of the orders values are 'easy'.  (Similarly for
# comp-head-is-not)

# It's possible that I could do this all in one step, but somehow it's
# easier to keep my undestanding of it if I break it out like this.



def specialize_word_order(hc,orders, mylang, ch, rules):

    adp = orders['adp']
    aux = orders['aux']
    qpart_order = orders['qpart_order'] #TODO: verify _-delimited key
    auxcomp = ch.get('aux-comp')
    wo = ch.get('word-order')
    auxorder = ch.get('aux-comp-order')

    if ch.get('has-aux') == 'yes':
        vcluster = determine_vcluster(auxcomp, auxorder, wo, ch)
    else:
        vcluster = False

    # ASF 2008-12-07 If verbal cluster is present, introduce relevant feature
    # and pass-up in lex-rule.
    # Also add relevant constraint to basic-head-comp-phrase

    if vcluster:
        mylang.add('cat :+ [ VC luk ].',
                   'Introducing VC keeps track whether main-verb is present in cluster',
                   section='addenda')
        mylang.add('lex-rule :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
                              DTR.SYNSEM.LOCAL.CAT.VC #vc ].',
                   section='addenda')
        mylang.add('basic-head-comp-phrase :+ [ SYNSEM.LOCAL.CAT.VC #vc, \
                       NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].',
                   section='addenda')
    # ERB 2006-09-15 First add head-comp or comp-head if they aren't
    # already there.  I don't think we have to worry about constraining
    # SUBJ or COMPS on these additional rules because they'll only be for
    # adp or auxv or qpart, so far

    if hc == 'comp-head' and (adp == 'ov-prep' or adp == 'ov-both' or aux == 'ov-auxv' or qpart_order == 'ov-qs'):
        mylang.add('head-comp-phrase := basic-head-1st-comp-phrase & head-initial.')


    if hc == 'head-comp' and (adp == 'vo-post' or adp == 'ov-both' or aux == 'vo-vaux' or qpart_order == 'vo-sq'):
        mylang.add('comp-head-phrase := basic-head-1st-comp-phrase & head-final.')

    # ASF: allowing for both prepositions and postpositions
    # if it is both HEADFINAL is used (for now) to register order of adposition
    if 'both' in adp:
        mylang.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL - ].')
        mylang.add('comp-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.HEADFINAL + ].')
        mylang.add('cat :+ [ HEADFINAL  bool].', comment='HEADFINAL registers whether a word precedes or follows its complement',section='features')

    # ASF 2008-11-18, special auxiliary rule that allows for auxiliaries
    # to combine with v's when aux-comp order is not harmonic
    # the vcluster + constraint is added to head-comp-phrase, since this
    # aux-rule is always combined with the verbal cluster analysis.

    if aux == 'auxv-rule':
        mylang.add('''aux-comp-phrase := basic-marker-comp-phrase & marker-initial-phrase &
                                   [ SYNSEM.LOCAL.CAT [ HEAD.FORM #vform,
                                                        VC #vc ],
                                     MARKER-DTR.SYNSEM.LOCAL.CAT.HEAD verb & [ AUX +,
                                                                               FORM #vform ],
                                     NON-MARKER-DTR.SYNSEM.LOCAL.CAT [ HEAD verb,
                                                             VC #vc ] ].''')
        mylang.add('comp-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
    if aux == 'vaux-rule':
        mylang.add('''comp-aux-phrase := basic-marker-comp-phrase & marker-final-phrase &
                                   [ SYNSEM.LOCAL.CAT [ HEAD.FORM #vform,
                                                        VC #vc ],
                                     MARKER-DTR.SYNSEM.LOCAL.CAT.HEAD verb & [ AUX +,
                                                                               FORM #vform ],
                                     NON-MARKER-DTR.SYNSEM.LOCAL.CAT [ HEAD verb,
                                                             VC #vc ] ].''')
        mylang.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')

    # add necessary restrictions to assure verb clusters
    # and special auxiliary rules for vso/osv and free word order.

    if vcluster:
        if wo == 'vso' or wo == 'free' or wo == 'v-initial':
            mylang.add('head-subj-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
        if wo == 'osv' or wo == 'free' or wo == 'v-final':
            mylang.add('subj-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
        if (aux == 'vini-vc' and aux == 'vo-auxv' ) or wo == 'free':
            mylang.add('head-comp-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
        if (aux == 'vfin-vc' and aux == 'ov-vaux') or wo == 'free':
            mylang.add('comp-head-phrase := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
        if wo == 'free' or wo == 'vso' or wo == 'osv':
            if auxorder == 'before' and aux != 'ov-auxv':
                mylang.add('aux-comp-phrase := basic-head-1st-comp-phrase & head-initial & \
                    [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ], \
                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD verb ].')
                aux = 'auxc'
            elif auxorder == 'after' and aux != 'vo-vaux':
                mylang.add('comp-aux-phrase := basic-head-1st-comp-phrase & head-final & \
                    [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ], \
                      NON-HEAD-DTR.SYNSEM.LOCAL.CAT.HEAD verb ].')
                aux = 'caux'
            if wo == 'free':
                mylang.add('head-comp-phrase-2 := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')
                mylang.add('comp-head-phrase-2 := [ HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].')

    # Add rules to rules.tdl when necessary

    if aux == 'ov-auxv' or adp == 'ov-prep' or adp == 'ov-both' or qpart_order == 'ov-qs':
        rules.add('head-comp := head-comp-phrase.')

    if aux == 'vo-vaux' or adp == 'vo-post' or adp == 'vo-both' or qpart_order == 'vo-sq':
        rules.add('comp-head := comp-head-phrase.')

    if aux == 'auxv-rule' or aux == 'auxc':
        rules.add('aux-comp := aux-comp-phrase.')

    if aux == 'vaux-rule' or aux == 'caux':
        rules.add('comp-aux := comp-aux-phrase.')

    # ERB 2006-09-15 AUX if we're going to mention it, so the tdl compiles.

    if aux != 'easy':
        mylang.add('head :+ [AUX bool].', section='addenda')

    # ERB 2006-10-05 Collect positive statements about head-comp/comp-head
    # We only need to do this is if the word order is not free, and we only
    # need to do it for one of head-comp or comp-head, depending on the order
    # of o and v.


    head_comp_is = []
    comp_head_is = []

    # VO order
    if aux == 'vo-vaux':
        comp_head_is.append('aux')
    if adp == 'vo-post' or adp == 'vo-both':
        comp_head_is.append('adp')
    if qpart_order == 'vo-sq':
        comp_head_is.append('comp')

    # OV order

    if aux == 'ov-auxv':
        head_comp_is.append('aux')
    if adp == 'ov-prep' or adp == 'ov-both':
        head_comp_is.append('adp')
    if qpart_order == 'ov-qs':
        head_comp_is.append('comp')

    # ERB 2006-10-05 Collect negative statements about head-comp/comp-head.
    # This needs to be done even if the word order is free.  It might need
    # to be done for both head-comp and comp-head in a free word order language
    # with inconsistent constraints on comp, adp, and aux.


    head_comp_is_not = []
    comp_head_is_not = []

    # when free word order has cluster, restriction on hc does not apply

    if vcluster and (aux == 'free-auxv' or aux == 'free-vaux'):
        aux = 'free-auxcl'

    if aux == 'free-auxv' or aux == 'ov-auxv' or aux == 'auxv-rule':
        comp_head_is_not.append('aux')
    if aux == 'free-vaux' or aux == 'vo-vaux' or aux == 'vaux-rule':
        head_comp_is_not.append('aux')
    if adp == 'free-prep' or adp == 'ov-prep':
        comp_head_is_not.append('adp')
    if adp == 'free-post' or adp == 'vo-post':
        head_comp_is_not.append('adp')
    if qpart_order == 'free-qs' or qpart_order == 'ov-qs':
        comp_head_is_not.append('comp')
    if qpart_order == 'free-sq'or qpart_order == 'vo-sq':
        head_comp_is_not.append('comp')


    # ERB 2006-10-05 Add constraints to head-comp/comp-head.

    # First the positive constraints.  We only have positive constraints if
    # the word order is not free. We should only have positive constraints for
    # one of head-comp and comp-head and negative constraints for the other.
    # In the free word order case, we might have negative constraints for each
    # one.

    # If only one part of speech type is allowed by the positive constraints,
    # we don't have a disjunctive type.

    # ASF for aux-comp and comp-aux, restriction should be AUX +
    # ASF for v-final and v-initial, added SUBJ < [] > constraint to form
    # a verbal cluster at the beginning or end of the sentence.
    # ASF 2009-04-21: should also be restricted to being a verb, if only one
    # non-standard order is present.


    if len(head_comp_is) == 1:
        head = head_comp_is[0]
        if head == 'aux':
            mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ] ].',
                       'head-comp-phrase requires auxiliary heads.')
            if wo == 'v-final' and auxcomp == 'vp':
                mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ ] > ].')
        else:
            mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
                       'head-comp-phrase requires things that are [ HEAD ' + head + ' ].')

    if len(comp_head_is) == 1:
        head = comp_head_is[0]
        if head == 'aux':
            mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ] ].',
                       'comp-head-phrase requires auxiliary heads.')
            if wo == 'v-initial' and auxcomp == 'vp':
                mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.VAL.SUBJ < [ ] > ].')
        else:
            mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
                       'comp-head-phrase requires things that are [ HEAD ' + head + ' ].')

    # Now the case where we do have disjunctive constraints.   NB: The order
    # here is important, since we're constructing the string which has to
    # correspond to the disjunctive head type.  While those are logically just
    # disjunctions, as far as the LKB is concerned, they're type names, so
    # we need an exact string match.

    # +nvjrcdmo.
    # +nvjrpdmo.


    if len(head_comp_is) > 1:
        head = '+'
        auxresthc = False
        if head_comp_is.count('aux'):
            head += 'v'
            auxresthc = True
        if head_comp_is.count('adp'):
            head += 'p'
        if head_comp_is.count('comp'):
            head += 'c'

        mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
                   'head-comp-phrase requires things that are one of: ' + str(head_comp_is))
        if auxresthc:
            mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].')

    if len(comp_head_is) > 1:
        head = '+'
        auxrestch = False
        if comp_head_is.count('aux'):
            head += 'v'
            auxrestch = True
        if comp_head_is.count('adp'):
            head += 'p'
        if comp_head_is.count('comp'):
            head += 'c'

        mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
                   'comp-head-phrase requires things that are one of: ' + str(head_comp_is))
        if auxrestch:
            mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX + ].')

    # Now the negative constraints.  This is where we extracted the
    # information that head-comp or comp-head can't be certain things.
    # Of course, in tdl we only have positive constraints.  So, we have
    # to translate the information to positive constraints.  Currently,
    # these will always involve disjunctive types, since we have more
    # pos types than things we're collecting negative info about.  The
    # other case is that we have no negative constraints, in which case
    # there's nothing to do.  We can just leave the HEAD value
    # completely underspecified.

    if head_comp_is_not.count('aux'):
        mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].')
        head_comp_is_not.remove('aux')

    if len(head_comp_is_not) > 0:
        head = '+n'
        if head_comp_is_not.count('verb') == 0:
            head += 'v'
        head += 'jr'
        if head_comp_is_not.count('adp') == 0:
            head += 'p'
        if head_comp_is_not.count('comp') == 0:
            head += 'c'
        head += 'dmo'

        mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
                   'The head of head-comp-phrase can\'t be: ' + str(head_comp_is_not))

    if comp_head_is_not.count('aux'):
        mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].')
        comp_head_is_not.remove('aux')

    if len(comp_head_is_not) > 0:
        head = '+n'
        if comp_head_is_not.count('verb') == 0:
            head += 'v'
        head += 'jr'
        if comp_head_is_not.count('adp') == 0:
            head += 'p'
        if comp_head_is_not.count('comp') == 0:
            head += 'c'
        head += 'dmo'

        mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD ' + head + ' ].',
                   'The head of comp-head-phrase can\'t be: ' + str(comp_head_is_not))


# ERB 2006-10-05 Below is what I had before I had to generalize because
# of addition of qpart_order.

# # The ~adp constraint and the ~comp constraint

#   if adp == 'ov-prep' or adp == 'free-prep':
#     mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].',
#                'comp-head-phrase is restricted from taking prepositions as its head.')

#   if adp == 'vo-post' or adp == 'free-post':
#     mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD +nvjrcdmo ].',
#                'head-comp-phrase is restricted from taking adpositions as its head.')


# # And the opposite cases (adp, V:AUX + | adp)

#   if adp == 'ov-prep' and aux == 'easy':
#     mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD adp ].',
#                'head-comp-phrase is only for prepositions.')

#   if adp == 'ov-prep' and aux == 'ov-auxv':
#     mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD +vp & [ AUX + ] ].',
#                'head-comp-phrase is only for prepositions and auxiliaries.')

#   if adp == 'vo-post' and aux == 'easy':
#     mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD adp ].',
#                'comp-head-phrase is only for postpositions.')

#   if adp == 'vo-post' and aux == 'vo-vaux':
#     mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD +vp & [ AUX + ] ].',
#                'comp-head-phrase is only for postpositions and auxiliaries.')

# # The [AUX -] constraint

#   if aux == 'vo-vaux' or aux == 'free-vaux':
#     mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].',
#                'head-comp-phrase is restricted from taking auxiliaries as its head.')

#   if aux == 'ov-auxv' or aux == 'free-auxv':
#     mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD.AUX - ].',
#                'comp-head-phrase is restricted from taking auxiliaries as its head.')

# # The v:AUX + constraint

#   if aux == 'ov-auxv' and adp == 'easy':
#     mylang.add('head-comp-phrase := [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ]].',
#                'head-comp-phrase is only for auxiliaries.')

#   if aux == 'vo-vaux' and adp == 'easy':
#     mylang.add('comp-head-phrase := [ SYNSEM.LOCAL.CAT.HEAD verb & [ AUX + ]].',
#                'comp-head-phrase is only for auxiliaries.')



# ERB 2006-09-15 Subroutine for handling NP rules.

def customize_np_word_order(mylang, ch, rules):

    if ch.get('has-dets') == 'yes':
        mylang.add(
            'head-spec-phrase := basic-head-spec-phrase.',
            'Rules for building NPs.  Note that the Matrix uses SPR for\n' +
            'the specifier of nouns and SUBJ for the subject (specifier) of verbs.')

        if ch.get('noun-det-order') == 'noun-det':
            mylang.add('head-spec-phrase := head-initial.')
        if ch.get('noun-det-order') == 'det-noun':
            mylang.add('head-spec-phrase := head-final.')

        rules.add('head-spec := head-spec-phrase.')


        # ERB 2006-09-14 I think that all languages have some form of
        # the Bare NP phrase.  Eventually there will be some choices about
        # this (given an appropriate module).  For now, use this stand in.

    mylang.add('bare-np-phrase := basic-bare-np-phrase &\
  [ C-CONT.RELS <! [ PRED \"exist_q_rel\" ] !> ].',
               'Bare NP phrase.  Consider modifying the PRED value of the quantifier relation\nintroduced to match the semantic effect of bare NPs in your language.')

    rules.add('bare-np := bare-np-phrase.')

# OZ 2017-11-13 A subroutine to create appropriate phrase structure rules
# for subordinate clauses, if needed. Currently only supports V2 matrix order
# with V-final subordinate order. If the subordinate word order is the same
# as in the matrix clause, no work needs to be done here.

def customize_subord_word_order(mylang,ch,wo,rules):
    if 'subord-word-order' in ch:
        if ch.get('subord-word-order') == 'vfinal' and wo == 'v2':
            mylang.add('subord-phrase := head-final &\n'
                       ' [ SYNSEM.LOCAL.CAT.MC #mc & - ,\n  HEAD-DTR.SYNSEM.LOCAL.CAT.MC #mc ].',
                       'Phrase structure rules for subordinate clauses')
            mylang.add('subord-comp-head-phrase := subord-phrase & basic-head-1st-comp-phrase.')
            mylang.add('subord-subj-head-phrase := subord-phrase & decl-head-subj-phrase.')

            rules.add('subord-comp-head := subord-comp-head-phrase.')
            rules.add('subord-subj-head := subord-subj-head-phrase.')

            # For German-like subordinate clauses, we need additional feature VC (verb cluster)
            # to ensure that verbs and auxiliaries in subordinate clauses cluster at the end.
            if 'has-aux' in ch and ch['has-aux'] == 'yes':
                update_lex_items_vcluster(ch, mylang)
                mylang.add('subord-phrase := [ SYNSEM.LOCAL.CAT.VC - ].')
                mylang.add('basic-unary-phrase :+\
                              [ SYNSEM.LOCAL.CAT.VC #vc,\
                                ARGS.FIRST.SYNSEM.LOCAL.CAT.VC #vc ].', merge=True, section='phrases')
                mylang.add('basic-binary-headed-phrase :+ '
                           '[ SYNSEM.LOCAL.CAT.VC #vc,'
                           'NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].', merge=True, section='phrases')

                mylang.add('verb-cluster-phrase := head-final & '
                           '[ SYNSEM.LOCAL.CAT [ VC +, MC #mc & - ], '
                           'HEAD-DTR.SYNSEM.LOCAL.CAT [ VC +, MC #mc], '
                           'NON-HEAD-DTR.SYNSEM.LOCAL.CAT.VC + ].',merge=True,section='phrases')
                mylang.add('vc-comp-head-phrase := verb-cluster-phrase & basic-head-1st-comp-phrase.',
                           merge=True,section='phrases')
                rules.add('vc-comp-head := vc-comp-head-phrase.')
                if ch['has-dets'] == 'yes':
                    mylang.add('basic-head-spec-phrase :+ [ SYNSEM.LOCAL.CAT.VC #vc,'
                               'HEAD-DTR.SYNSEM.LOCAL.CAT.VC #vc ].')

# ERB 2006-09-14 Subroutine for figuring out the relationship of major
# constituent order to adpositions and auxiliaries.  Returns two values:
# for adp and aux.  It takes in the values of wo and hc determined in
# the course of creating the basic word order rules.



def determine_consistent_order(wo,hc,ch):

    adp = 'easy'
    aux = 'easy'
    qpart_order = 'easy'

    # Is the ordering of adpositions consistent with the ordering of O and V?
    # Assuming that adpositions are consistent within a language (i.e., you won't
    # find subject postpositions and object prepositions).


    # ASF: bug fix, adporder is whatever adporder was defined last
    # which excludes the possibility to have both prepositions and adpositions
    adporders = []
    for adp in ch.get('adp',[]):
        adp_order = adp.get('order')
        if not adp_order in adporders:
            adporders.append(adp_order)

    # ERB 2006-10-05 Fixing bug in free word order case.
    adporder = ''
    if len(adporders) == 2:
        adporder = 'both'
    elif len(adporders) == 1:
        adporder = adporders[0]

    if adporder:
        if wo == 'free':
            if adporder == 'before':
                adp = 'free-prep'
            elif adporder == 'after':
                adp = 'free-post'
            elif adporder == 'both':
                adp = 'free-both'
        elif hc == 'comp-head':
            if adporder == 'before':
                adp = 'ov-prep'
            elif adporder == 'both':
                adp = 'ov-both'
        elif hc == 'head-comp':
            if adporder == 'after':
                adp = 'vo-post'
            elif adporder == 'both':
                adp = 'vo-both'
    # Now what about auxiliaries?
    # ASF 2008-12-07 for non-harmonic order and v (not vp) comps,
    # we need a different procedure (see if auxcomp...)

    if ch.get('has-aux') == 'yes':
        auxcomp = ch.get('aux-comp')
        if wo == 'free':
            if ch.get('aux-comp-order') == 'before':
                aux = 'free-auxv'
            elif ch.get('aux-comp-order') == 'after':
                aux = 'free-vaux'
        elif hc == 'comp-head' and ch.get('aux-comp-order') == 'before':
            if auxcomp == 'v':
                aux = 'auxv-rule'
            else:
                aux = 'ov-auxv'
        elif hc == 'head-comp' and ch.get('aux-comp-order') == 'after':
            if auxcomp == 'v':
                aux = 'vaux-rule'
            else:
                aux = 'vo-vaux'

    # ERB 2006-10-05 And what about the order of question particles wrt
    # to other kinds of head-comp?  I'm assuming for now that question particles
    # and other complementizers will behave the same way.  Are there languages
    # in which that is not true?

    if ch.get('q-part-order'):
        if wo == 'free':
            if ch.get('q-part-order') == 'after':
                qpart_order = 'free-sq'
            elif ch.get('q-part-order') == 'before':
                qpart_order = 'free-qs'
        elif hc == 'comp-head' and ch.get('q-part-order') == 'before':
            qpart_order = 'ov-qs'
        elif hc == 'head-comp' and ch.get('q-part-order') == 'after':
            qpart_order = 'vo-sq'

            # return what we learned

    return {'adp': adp, 'aux': aux, 'qpart_order': qpart_order} #TODO: verify key


