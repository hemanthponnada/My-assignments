/*TASK 1: UNDERSTANDING THE DATA IN HAND
	
    The following superstotesdb consists of the detials about customers, Products, Orders placed by them, shipping details
    This database contains five tables Cust_dimen, Orders_dimen, Prod_dimen, Shipping_dimen, Market_fact. 
    The market_fact table is the central table which contains all the details about which customer has purchased which product,
    what is his order number, what is his shipping id, the quantity purchased by the particular customer, the discount he got for the
    partcular product that he has purchased. 
    
    
    1. Cust_dimen 	  -   Contains details about the name of customers, region, customer segment
						  Customer_Name 	- varchar
                          Province 			- varchar
                          Region			- varchar
                          Customer_segment	- varchar
                          Cust_id			- varchar
						  The Primary key in this table is cust_id. There are no foreign keys.
 	2. Orders_dimen   -   Contains details about the order id, order_date, priority
						  Order_id         - integer
                          Order_date	   - Date
                          Ord_id		   - varchar
						  The primary key in this table is Ord_id. There are no foreign keys. 
	3. Prod_dimen     -   This table contains the details about the Product id, Product category, Product sub categories
						  Product_category  	- varchar
                          Product_subcategory	- varchar
                          Product_id			- varchar
						  The primary key in this table is Prod_id. There are no foreign keys
	4. Shipping_dimen -   This table contains details about the order id, Shipment mode, Shipment date, Shipment id.
						  Order_id		- varchar
                          Shipment_mode - varchar
                          Shipment_date - date
                          Shipment_id 	- varchar
						  The primary key in this table is Ship_id. There are no foreign keys.
	5. Market_fact    -   This table contains all the complete details of the Order id, Product id, Shipment id, Customer id, Sales, discount, Order quantity, Profit, Shipping
						  Ord_id			  - varchar
                          Prod_id			  - varchar
                          Ship_id			  - varchar
                          Cust_id			  - varchar
                          Sales				  - Float
                          Discount			  - Float
                          Order_quantity	  - Integer
                          Profit 			  - Float
                          Shipping_cost		  - Float
                          Product_base_margin - Float
                          There are four foreign keys in this table. Ord_id, Prod_id, Ship_id, Cust_id. No primary keys. 
*/

/* TASK 2: BASIC ANALYSIS
  A.  Find the total and average sales(Display total_sales and avg_sales */
		SELECT ROUND(SUM(Sales),2) Total_sales, ROUND(AVG(Sales),2) Avg_sales FROM market_fact;

/*B.  Display the number of customers in each region in decreasing order of
	  no_of_customers. The result should contain columns Region, no_of_customers*/
		SELECT Region,COUNT(*) no_of_customers 
		FROM cust_dimen 
		GROUP BY Region 
		ORDER BY no_of_customers DESC;

/*C.  Find the region having maximum customers (display the region name and
      max(no_of_customers)*/
		SELECT Region, COUNT(*) no_of_customers 
		FROM cust_dimen 
		GROUP BY Region 
		ORDER BY no_of_customers DESC LIMIT 1 ;

/*D.  Find the number and id of products sold in decreasing order of products sold (display
	  product id, no_of_products sold)*/
		SELECT Prod_id, SUM(Order_Quantity) no_of_products_sold 
        FROM market_fact 
        GROUP BY Prod_id 
        ORDER BY no_of_products_sold DESC;
      
/*E.  Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and
	  the number of tables purchased (display the customer name, no_of_tables
	  purchased)*/ 
		SELECT c.Customer_Name, m.Order_Quantity
		FROM cust_dimen c  
		JOIN market_fact m ON c.Cust_id=m.Cust_id JOIN Prod_dimen p ON p.Prod_id=m.Prod_id 
        WHERE c.Region='Atlantic' AND p.Product_sub_category='Tables';

/*Task 3: Advanced Analysis
A.    Display the product categories in descending order of profits (display the product
	  category wise profits i.e. product_category, profits)?*/
		SELECT p.Product_Category, ROUND(SUM(m.Profit),2) Profit
		FROM prod_dimen p JOIN market_fact m ON p.Prod_id=m.Prod_id
		GROUP BY p.Product_Category
		ORDER BY SUM(m.Profit) DESC;

/*B.  Display the product category, product sub-category and the profit within each subcategory
      in three columns.*/  
		SELECT p.Product_Category, p.Product_sub_Category,m.Profit
		FROM prod_dimen p 
		JOIN market_fact m ON p.Prod_id=m.Prod_id
 
/*C. Where is the least profitable product subcategory shipped the most? For the least
	 profitable product sub-category, display the region-wise no_of_shipments and the
	 profit made in each region in decreasing order of profits (i.e. region,
     no_of_shipments, profit_in_each_region)
	 o Note: You can hardcode the name of the least profitable product subcategory*/
	 
     #In the where clause a sub-query is written to find out product id of the least profitable product. 
		SELECT c.Region, COUNT(m.Ship_id) No_of_shipments, ROUND(SUM(m.Profit),2) Profit 
		FROM market_fact m JOIN cust_dimen c ON c.Cust_id=m.Cust_id 
		WHERE m.Prod_id=(SELECT Prod_id FROM market_fact WHERE Profit= (SELECT MIN(Profit) FROM market_fact)) 
		GROUP BY c.Region ORDER BY profit DESC ;