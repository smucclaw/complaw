type Props_Example_A_1 = {
    "used" : number;
    "standard" : string;
    "has" : number;
}
type Return_Example_A_1 = {
    "deliver" : boolean;
    "charge" : string;
}
export function Example_A_1 ( props : Props_Example_A_1 ) : Return_Example_A_1 {
  if (props["used"] >=0.5 && props["standard"]==="standard" && props["has"] >=100.0) { // 1
    return {"deliver":true, "charge":"free"};
  }
  else if (props["used"] >=0.5 && props["standard"]==="non-standard") { // 2
    return {"deliver":true, "charge":"charged"};
  }
  else if (props["used"] >=0.5 && props["standard"]==="standard") { // 3
    return {"deliver":true, "charge":undefined};
  }
  else if ("default") { // 4
    return {"deliver":undefined, "charge":undefined};
  }
}

type Props_Example_A_2 = {
    "Box Type" : string;
    "Grocery Order Sum" : number;
    "Filling Degree" : number;
}
type Return_Example_A_2 = {
    "Delivery Status" : string;
}
export function Example_A_2 ( props : Props_Example_A_2 ) : Return_Example_A_2 {
  if (props["Box Type"]==="Standard" && props["Grocery Order Sum"] > 100.0 && props["Filling Degree"] > 50.0) { // 1
    return {"Delivery Status":"free"};
  }
  else if (props["Box Type"]==="Standard" && props["Filling Degree"] > 50.0) { // 2
    return {"Delivery Status":"charged"};
  }
  else if (props["Box Type"]==="Standard") { // 3
    return {"Delivery Status":"not offered"};
  }
  else if (props["Box Type"]==="Non-Standard" && props["Filling Degree"] > 50.0) { // 4
    return {"Delivery Status":"charged"};
  }
  else if (props["Box Type"]==="Non-Standard") { // 5
    return {"Delivery Status":"not offered"};
  }
}

type Props_Example_B_1 = {
    "used" : number;
    "standard" : string;
    "has" : number;
}
type Return_Example_B_1 = {
    "deliver" : boolean;
    "charge" : string;
}
export function Example_B_1 ( props : Props_Example_B_1 ) : Return_Example_B_1 {
  if (props["used"] > 0.5 && props["standard"]==="standard" && props["has"] >=100.0) { // 1
    return {"deliver":true, "charge":"free"};
  }
  else if (props["standard"]==="standard") { // 2
    return {"deliver":true, "charge":undefined};
  }
  else if (props["used"] > 0.5 && props["standard"]==="non-standard") { // 3
    return {"deliver":true, "charge":"charged"};
  }
  else if ("default") { // 4
    return {"deliver":undefined, "charge":undefined};
  }
}

