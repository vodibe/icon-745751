# fmt: off
# cpt già conosciute
BN_KNOWN_CPTS = [
    TabularCPD(
        variable="page_menu_or",
        variable_card=len(defs.DS_DISCRETE_MAPPING_DEFAULT["page_menu_or"][1]),
        evidence=["page_template"],
        evidence_card=[
            len(defs.DS_DISCRETE_MAPPING_DEFAULT["page_template"][1])
        ],
        state_names={
            feature: state_names
            for feature, state_names in BN_STATE_NAMES.items()
            if feature in ["page_menu_or", "page_template"]
        },
        values=[  # P(page_menu_or = x | page_template)
            [0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.1],
            [0.97, 0.02, 0.04, 0.02, 0.97, 0.93, 0.93, 0.97, 0.3],
            [0.01, 0.02, 0.05, 0.02, 0.01, 0.01, 0.01, 0.01, 0.3],
            [0.01, 0.95, 0.90, 0.95, 0.01, 0.05, 0.05, 0.01, 0.3],
            #[0.98, 0.02, 0.05, 0.02, 0.98, 0.93, 0.94, 0.98, 0.33],
            #[0.01, 0.02, 0.05, 0.02, 0.01, 0.01, 0.01, 0.01, 0.33],
            #[0.01, 0.96, 0.90, 0.96, 0.01, 0.06, 0.05, 0.01, 0.34],
        ]
    ),
    TabularCPD(
        variable="page_ungrouped_multim",
        variable_card=len(defs.DS_DISCRETE_MAPPING_DEFAULT["page_ungrouped_multim"][1]),
        evidence=["page_template", "_designer_taste"],
        evidence_card=[
            len(defs.DS_DISCRETE_MAPPING_DEFAULT["page_template"][1]),
            len(defs.DS_DISCRETE_MAPPING_DEFAULT["_designer_taste"][1])
        ],
        state_names={
            feature: state_names
            for feature, state_names in BN_STATE_NAMES.items()
            if feature in ["page_ungrouped_multim", "page_template", "_designer_taste"]
        },
        values=[
            # 11     12   21     22   31    32     41   42    51     52   61    62    71    72    81    82    91     92
            [0.45, 0.70, 0.22, 0.65, 0.22, 0.65, 0.22, 0.65, 0.60, 0.85, 0.30, 0.50, 0.42, 0.78, 0.08, 0.08, 0.2, 0.3],
            [0.45, 0.20, 0.55, 0.25, 0.55, 0.25, 0.55, 0.25, 0.40, 0.15, 0.50, 0.48, 0.48, 0.21, 0.42, 0.50, 0.2, 0.3],
            [0.05, 0.07, 0.22, 0.05, 0.22, 0.05, 0.22, 0.05, 0.00, 0.00, 0.15, 0.01, 0.05, 0.01, 0.42, 0.40, 0.3, 0.2],
            [0.05, 0.03, 0.01, 0.05, 0.01, 0.05, 0.01, 0.05, 0.00, 0.00, 0.05, 0.01, 0.05, 0.00, 0.08, 0.02, 0.3, 0.2]
        ],
        
    ),
]
# fmt: on
