UPDATE metric SET required = false FROM category WHERE category = category.id AND category.name = 'context' AND metric.name = 'language';
