# order: {
#   user_id: id or none if anon user,
#   status: pending, paid, shipped, cancelled,
#   created_at: Date of placing the order,
#   shipping_method: there are 3,
#   total: cost in PLN,
#   we can check if billing and delivery details are equal
# }
# order_item: {
#   order_id: we can count avg number of items in order,
#   variant_id: id of variant of a product. we can look for popular products/variants,
#   quantity: number,
#   personalization_mask: we can check if user provided personalization,
#   price: actual price (after discount),
#   design_file_id: image provided by user to create personalization or none
# }

# discount: {
#   offer_id: number,
#   created_at: Date,
#   untill: Date,
#   price: new price
# }

# Every offer has categories and subcategories, reviews (we can look for a grade and number of reviews)
# Every variant has images (we can count them)

# NOTE: We can check if the ordered item was on sale, it may be important to track correlation of special offers with number of sold products

# I want to generate data with the specified schema
# Use smart data generation techniques to create realistic data
# Poisson distribution for quantities
# Price per product is a custom distribution, with only a few values
# Use Faker library to generate realistic data

import random
import json
from datetime import datetime, timedelta
from faker import Faker
import numpy as np
from scipy.stats import poisson
import pandas as pd
from typing import List, Dict, Any
from tqdm import tqdm
from pathlib import Path

# Set random seed for reproducibility
random.seed(42)
fake = Faker()
Faker.seed(42)
np.random.seed(42)
# Constants
NUM_USERS = 100
NUM_ORDERS = 300
NUM_ORDER_ITEMS = 1100
NUM_DISCOUNTS = 3
NUM_OFFERS = 20
NUM_CATEGORIES = 7
NUM_SUBCATEGORIES = 10
NUM_REVIEWS = 50
NUM_VARIANTS = 100
NUM_IMAGES = 10
NUM_SHIPPING_METHODS = 3

# Generate user data
def generate_users(num_users: int) -> List[Dict[str, Any]]:
    users = []
    for _ in range(num_users):
        user = {
            "user_id": fake.uuid4()
        }
        users.append(user)
    return users

# Generate categories and subcategories
def generate_categories(num_categories: int) -> List[Dict[str, Any]]:
    categories = []
    for o in range(num_categories):
        category = {
            "category_id": o,
            "name": fake.word()
        }
        categories.append(category)
    return categories

def generate_subcategories(num_subcategories: int, categories: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    subcategories = []
    for i in range(num_subcategories):
        subcategory = {
            "subcategory_id": i,
            "name": fake.word(),
            "category_id": random.choice(categories)["category_id"]
        }
        subcategories.append(subcategory)
    return subcategories

# Generate offer data
def generate_price() -> float:
    # Custom distribution for price
    weighted_prices = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
    weights = [0.05, 0.1, 0.15, 0.2, 0.25, 0.1, 0.05, 0.03, 0.02, 0.01]
    # Normalize weights
    weights = [w / sum(weights) for w in weights]
    return np.random.choice(weighted_prices, p=weights)

def generate_number_of_subcategories() -> int:
    # Use poisson distribution to generate the number of subcategories
    # with a mean of 2
    return poisson.rvs(mu=2)

def generate_offers(num_offers: int, subcategories: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    offers = []
    
    for i in range(num_offers):
        recommended = random.choices(["no", "landing_carousel", "additional_carousel", "in_category"],
                                                weights=[0.9, 0.025, 0.05, 0.025], k=1)[0]
        convertion_rate = random.normalvariate(0.05, 0.05)
        if recommended == "landing_carousel":
            convertion_rate = random.normalvariate(0.1, 0.05)
        elif recommended == "additional_carousel":
            convertion_rate = random.normalvariate(0.07, 0.05)
        elif recommended == "in_category":
            convertion_rate = random.normalvariate(0.08, 0.05)
        convertion_rate = max(0.01, min(convertion_rate, 1))  # Ensure convertion rate is between 0 and 1
        offer = {
            "offer_id": i,
            "created_at": fake.date_time_this_year(),
            "price": generate_price(),
            "subcategories": random.sample([sub["subcategory_id"] for sub in subcategories], k=generate_number_of_subcategories()),
            "recommended": recommended,
            "convertion_rate": convertion_rate
        }
        offers.append(offer)
    return offers

def generate_variants(num_variants: int, offers: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    variants = []
    # For every offer generate at least one variant
    for i, offer in enumerate(offers):
        variant = {
            "variant_id": i,
            "offer_id": offer["offer_id"],
            "image_count": random.randint(1, NUM_IMAGES)
        }
        variants.append(variant)
        
    # Generate additional variants
    for i in range(num_variants - len(offers)):
        variant = {
            "variant_id": i + len(offers),
            "offer_id": random.choice(offers)["offer_id"],
            "image_count": random.randint(1, NUM_IMAGES)
        }
        variants.append(variant)
    return variants

# Generate order data
def generate_order_items(order_id: int, variants: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    order_items = []
    # Generate a random number of order items for each order
    num_items = poisson.rvs(mu=3)
    variant = random.choice(variants)
    variant_id = variant["variant_id"]
    offer_id = variant["offer_id"]
    for _ in range(num_items):
        order_item = {
            "order_id": order_id,
            "variant_id": variant_id,
            "offer_id": offer_id,
            "quantity": poisson.rvs(mu=5),
            "personalization_mask": True if random.random() < 0.4 else False,
            "price": generate_price(),
            "design_file_id": True if random.random() < 0.3 else False,
            "was_discounted": False
        }
        order_items.append(order_item)
            
    return order_items

def generate_orders(num_orders: int, users: List[Dict[str, Any]], variants: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    orders = []
    order_items_all = {}
    for i in range(num_orders):
        order_items = generate_order_items(i, variants)
        order_items_all[i] = order_items
        order = {
            "order_id": i,
            "user_id": random.choice(users)["user_id"] if random.random() < 0.77 else None,
            "status": random.choices(["pending", "paid", "shipped", "delivered", "cancelled"],
                                    weights=[0.05, 0.1, 0.05, 0.75, 0.05], k=1)[0],
            "created_at": fake.date_time_this_year(),
            "shipping_method": random.choices(["paczkomat_inpost", "kurier_dpd", "kurier_inpost"],
                                            weights=[0.5, 0.3, 0.2], k=1)[0],
            "billing_and_delivery_details_equal": True if random.random() < 0.8 else False,
            "total": int(sum([item["price"] * item["quantity"] for item in order_items]))
        }
        # Add delivery cost to total
        if order["shipping_method"] == "paczkomat_inpost":
            order["total"] += 10
        elif order["shipping_method"] == "kurier_dpd":
            order["total"] += 20
        elif order["shipping_method"] == "kurier_inpost":
            order["total"] += 15
        orders.append(order)
    return orders, order_items_all

# Generate discount data
def generate_discounts(num_discounts: int, offers: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    discounts = []
    date_of_first_offer_created = min(offer["created_at"] for offer in offers)
    today = datetime.now()
    
    while len(discounts) < num_discounts:
        # Generate a random discount
        start_date = fake.date_time_between_dates(date_of_first_offer_created, today)
        end_date = start_date + timedelta(days=random.randint(10, 50))
        offer_id = random.choice(offers)["offer_id"]
        default_price = next(offer["price"] for offer in offers if offer["offer_id"] == offer_id)
        
        # Ensure the discount price is lower than the original price
        discount_percentage = random.uniform(0.1, 0.4)
        discount_price = round(default_price * (1 - discount_percentage), 2)
        discount = {
            "offer_id": random.choice(offers)["offer_id"],
            "created_at": start_date,
            "until": end_date,
            "price": discount_price
        }
        
        # Check if there is a discount with the same offer_id and overlapping dates
        if any(d["offer_id"] == discount["offer_id"] and
                (d["created_at"] < discount["until"] and d["until"] > discount["created_at"]) and
                (discount["created_at"] < d["until"] and discount["until"] > d["created_at"])
                for d in discounts):
            continue
        
        discounts.append(discount)
        
    return discounts

# Apply discounts to orders
def apply_discounts_to_orders(orders: List[Dict[str, Any]], order_items_all: Dict[int, List[Dict[str, Any]]], discounts: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    for order in orders:
        # Get all discounts where order.created_at is between discount created_at and until
        applicable_discounts = [discount for discount in discounts if order["created_at"] > discount["created_at"] and order["created_at"] < discount["until"]]
        
        # Apply discounts to order items
        order_items = order_items_all[order["order_id"]]
        for order_item in order_items:
            # Check if the order item variant_id is in any of the applicable discounts
            for discount in applicable_discounts:
                if order_item["offer_id"] == discount["offer_id"]:
                    # Apply discount to the order item price
                    order_item["price"] = discount["price"]
                    order_item["was_discounted"] = True
                    break
        order_items_all[order["order_id"]] = order_items
        
        # Recalculate the total price of the order
        order["total"] = sum(item["price"] * item["quantity"] for item in order_items)
        # Add delivery cost to total
        if order["shipping_method"] == "paczkomat_inpost":
            order["total"] += 10
        elif order["shipping_method"] == "kurier_dpd":
            order["total"] += 20
        elif order["shipping_method"] == "kurier_inpost":
            order["total"] += 15
            
    return orders, order_items_all
            
def datetime_to_str(date: datetime) -> str:
    return date.strftime("%Y-%m-%d %H:%M:%S")

# Generate data
def generate_data():
    users = generate_users(NUM_USERS)
    categories = generate_categories(NUM_CATEGORIES)
    subcategories = generate_subcategories(NUM_SUBCATEGORIES, categories)
    offers = generate_offers(NUM_OFFERS, subcategories)
    variants = generate_variants(NUM_VARIANTS, offers)
    orders, order_items_all = generate_orders(NUM_ORDERS, users, variants)
    discounts = generate_discounts(NUM_DISCOUNTS, offers)
    
    # Apply discounts to orders
    orders, order_items_all = apply_discounts_to_orders(orders, order_items_all, discounts)
    
    # Convert datetime to string
    orders = [
        {**order, "total": int(order["total"]), "created_at": datetime_to_str(order["created_at"])} for order in orders
    ]
    offers = [
        {**offer, "price": int(offer["price"]), "created_at": datetime_to_str(offer["created_at"])} for offer in offers
    ]
    discounts = [
        {**discount, "created_at": datetime_to_str(discount["created_at"]), "until": datetime_to_str(discount["until"])} for discount in discounts
    ]
    
    order_items = []
    for order_id, items in order_items_all.items():
        for item in items:
            order_items.append({
                **item,
                "price": int(item["price"])
            })
    
    return {
        "users": users,
        "categories": categories,
        "subcategories": subcategories,
        "offers": offers,
        "variants": variants,
        "orders": orders,
        "order_items": order_items,
        "discounts": discounts
    }
    
# Save data to JSON files
def save_data(data: Dict[str, Any], output_dir: str) -> None:
    Path(output_dir).mkdir(parents=True, exist_ok=True)
    for key, value in data.items():
        with open(f"{output_dir}/{key}.json", "w") as f:
            json.dump(value, f, indent=4)
    
# Main function
if __name__ == "__main__":
    data = generate_data()
    
    # Save data to JSON files
    output_dir = "generated_data"
    save_data(data, output_dir)
    
    print(f"Data generated and saved to {output_dir}")