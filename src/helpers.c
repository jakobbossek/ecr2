/* Force value to stay within box constraints.
 *
 * @param x [numeric(1)]
 *   Numeric value.
 * @param lower [numeric(1)]
 *   Lower box constraint.
 * @param upper [numeric(1)]
 *   Upper box constraint.
 * @return [numeric(1)]
 */
double forceToBounds(const double x, const double lower, const double upper) {
  if (x < lower) {
    return(lower);
  } else if (x > upper) {
    return(upper);
  }
  return(x);
}
