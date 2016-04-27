deepblue.create_column_type_calculated(name = "CALC_SQUARE_ROOT_VALUE",
  description ="The square root of the value column",
  code = "return sqrt(value_of('VALUE'))", user_key = "my_secret_key")