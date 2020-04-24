# Language Spec
## Equals Function
* If no pattern -> SQL bool
* If pattern in second argument 
-> Like
### `like`

### Or/Not

### If/Then in some way 

### `unique_occurances`

### `let` declerations 
* How to do this in SQL?
* Change all 'SELECT *' final statments to use fresh aliases
* add new alias in from - not nested!

* Variables CANNOT begin with underscore

### cart_product 
* A way to start out a query with multiple tables 

### `delete`

### `insert`

### filter_min, filter_max 
* ```filter_max attribute_lst attribute table_expression``` is the table containing only the row where attribute `attribute` is minimized projected on the attributes `attribute_lst` 
* SQL example: `Select ProductName, min(Price) as Price FROM PRODUCTS`
### count_instances 
* `count_instances attribute_lst table_expression` is the table containing each unique tuple grouped by `attribute_lst` with the number of times that tuple occured. The resulting table has the attributes in `attribute_lst` plus an aditional attribute `count`
* SQL example: `SELECT SupplierID, CategoryID, count(*) as count
FROM Products Group By SupplierID, CategoryID; ` 