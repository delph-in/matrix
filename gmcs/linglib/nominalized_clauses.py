def create_pseudo_features(ch):
    """
    a function to create pseudo features from the defined nominalization strategies
    """
    for ns in ch.get('ns'):
        nsname = ns.get('name')
        for vpc in ch['verb-pc']:
            for lrt in vpc['lrt']:
                for feat in lrt['feat']:
                    if feat['name'] == 'nominalization':
                        if nsname not in feat['name']['nominalization']:
                            feat['name']['nominalization'].append(nsname)
                    else:
                        feat['name']['nominalization'] = [nsname]

def customize_nmcs(mylang, ch, rules, irules):
    """
    the main nominalized clause customization routine
    """
    create_pseudo_features(ch)