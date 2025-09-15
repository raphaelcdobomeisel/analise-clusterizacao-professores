def dic_cols(name_col, dictionary):
    """
    Returns the description of a column from the dictionary DataFrame.
    
    Parameters:
    - name_col (str): name of the column to search
    - dictionary (pd.DataFrame): DataFrame with NAME_COL and DESC
    
    Returns:
    - str: description of the column, or 'Column not found'
    """
    row = dictionary.loc[dictionary["NAME_COL"] == name_col, "DESC"]
    if not row.empty:
        return row.values[0]
    else:
        return "Column not found"
