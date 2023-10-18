book_stack = ["Harry Potter and the Prisoner of Azkaban",
              "Gone With the Wind",
              "Frankenstein or The Modern Prometheus",
              "Band of Brothers",
              "The Caves of Steel",
              "The Grapes of Wrath",
              "Ubik"]

replacement_count = 1
for book_index in range(0, len(book_stack)):
    try:
        curr_book, next_book = book_stack[book_index], book_stack[book_index + 1]
    except KeyError:
        # the books were in exact reversed order
        # i.e. the entire stack fell
        return len(book_stack)

    if curr_book < next_book:
        return replacement_count
        break
    else:
        replacement_count += 1

"""
edge cases:
undisturbed book pile
single book
completely reversed pile
"""

book_stack_reg = ["Harry Potter and the Prisoner of Azkaban",
                  "Gone With the Wind",
                  "Frankenstein or The Modern Prometheus",
                  "Band of Brothers",
                  "The Caves of Steel",
                  "The Grapes of Wrath",
                  "Ubik"]


book_stack_undisturbed = ["Band of Brothers",
                          "Frankenstein or The Modern Prometheus",
                          "Gone With the Wind",
                          "Harry Potter and the Prisoner of Azkaban",
                          "The Caves of Steel",
                          "The Grapes of Wrath",
                          "Ubik"]


def find_replacements(books: list[str]) -> int:
    replacement_count = 1

    for book_index in range(0, len(books)):
        try:
            curr_book, next_book = books[book_index], books[book_index + 1]
        except IndexError:
            # the books were in exact reversed order
            # i.e. the entire stack fell
            return len(books)

        if curr_book < next_book:
            if replacement_count == 1:
                return 0
            return replacement_count
        else:
            replacement_count += 1
    return replacement_count


print(find_replacements(book_stack_reg))
print(find_replacements(book_stack_undisturbed))
print(find_replacements(list(reversed(book_stack_undisturbed))))
