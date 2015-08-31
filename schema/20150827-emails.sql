UPDATE account SET email = left(email, position('@' in email)) || lower(right(email, -position('@' in email)));
