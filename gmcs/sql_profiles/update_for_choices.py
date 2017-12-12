#################################################################
# Read in a list of specific filters, update the filter, feat_group
# and fltr_group tables appropriately, then filter the possibly
# grammatical strings and put results in res_sfltr.

from run_specific_filters import \
    update_tables_for_filters, filter_list

update_tables_for_filters(filter_list)
