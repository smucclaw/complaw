export function Example_A_1 ( used, standard, has ) {
  if (used >=0.5 && standard==="standard" && has >=100.0) { // 1
    return {"deliver":true, "charge":"free"};
  }
  else if (used >=0.5 && standard==="non-standard") { // 2
    return {"deliver":true, "charge":"charged"};
  }
  else if (used >=0.5 && standard==="standard") { // 3
    return {"deliver":true, "charge":undefined};
  }
  else if ("default") { // 4
    return {"deliver":undefined, "charge":undefined};
  }
}

export function Example_A_2 ( Box_Type, Grocery_Order_Sum, Filling_Degree ) {
  if (Box_Type==="Standard" && Grocery_Order_Sum > 100.0 && Filling_Degree > 50.0) { // 1
    return {"Delivery Status":"free"};
  }
  else if (Box_Type==="Standard" && Filling_Degree > 50.0) { // 2
    return {"Delivery Status":"charged"};
  }
  else if (Box_Type==="Standard") { // 3
    return {"Delivery Status":"not offered"};
  }
  else if (Box_Type==="Non-Standard" && Filling_Degree > 50.0) { // 4
    return {"Delivery Status":"charged"};
  }
  else if (Box_Type==="Non-Standard") { // 5
    return {"Delivery Status":"not offered"};
  }
}

export function Example_B_1 ( used, standard, has ) {
  if (used > 0.5 && standard==="standard" && has >=100.0) { // 1
    return {"deliver":true, "charge":"free"};
  }
  else if (standard==="standard") { // 2
    return {"deliver":true, "charge":undefined};
  }
  else if (used > 0.5 && standard==="non-standard") { // 3
    return {"deliver":true, "charge":"charged"};
  }
  else if ("default") { // 4
    return {"deliver":undefined, "charge":undefined};
  }
}

