class SearchException implements Exception {
  final String message;

  const SearchException(this.message);

  @override
  String toString() => message;
}
