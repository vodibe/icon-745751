def create_bn(estimator_id="MLE", name=None) -> BayesianNetwork:
    """Crea la BN di default, la salva su file e restituisce l'oggetto BayesianNetwork.

    Args:
        - estimator_id (optional): Id stimatore. Default: "MLE".
        - name (optional): Nome della BN. Default: None.

    Returns:
        - BayesianNetwork.
    """

    # operazioni su dataset
    # leggi dataset
    print("Reading dataset...")
    ds = pd.read_csv(defs.ds3_gt_no_noise_path)
    bn_features_excluded = defs.ds3_features_excluded + [
        "page_load_time_ms",
        "page_width",
        "task4",
        "task5",
        "task6",
        "task7",
        "task8",
    ]
    ds = ds.drop(bn_features_excluded, axis=1)

    # discretizza perchè pgmpy supporta inferenza su distribuzioni discrete
    discretize_dataset(
        ds=ds,
        feature_domains=defs.ds3_gt_feature_domains,
        mapping=defs.DS_DISCRETE_MAPPING_DEFAULT,
    )

    # crea bn
    print("Creating BN structure...")
    bn = BayesianNetwork(ebunch=BN_EDGES_DEFAULT)

    # apprendimento parametri (cpd) della bn
    print("Learning BN parameters...")

    if estimator_id == "MLE":
        bn.fit(ds, estimator=MaximumLikelihoodEstimator, state_names=BN_STATE_NAMES)
    elif estimator_id == "K2":
        bn.fit(
            ds, estimator=BayesianEstimator, prior_type="K2", state_names=BN_STATE_NAMES
        )
    elif estimator_id == "BDeu":
        bn.fit(
            ds,
            estimator=BayesianEstimator,
            prior_type="BDeu",
            equivalent_sample_size=20,
            state_names=BN_STATE_NAMES,
        )
    else:
        return None

    if name:
        bn.name = f"{name}.bif"
    else:
        bn.name = f"bn_estimator_{estimator_id}.bif"

    # controlla bn
    if bn.check_model():
        print("Saving...")
        bif_writer = BIFWriter(bn)
        bif_writer.write_bif(defs.DIR_BIF / bn.name)

    return bn
