type Props_Example_2 = {
    "Season" : string;
    "Guest Count" : number;
    "Dish" : string;
    "Annotation" : any;
}
export function Example_2 ( props : Props_Example_2 ) {
  if (props["Season"]==="Fall" && props["Guest Count"] <=8.0) { // 1
    return {"Dish":"Spareribs"};
  }
  else if (props["Season"]==="Winter" && props["Guest Count"] <=8.0) { // 2
    return {"Dish":"Roastbeef"};
  }
  else if (props["Season"]==="Spring" && props["Guest Count"] <=4.0) { // 3
    return {"Dish":"Dry Aged Gourmet Steak"};
  }
  else if (props["Season"]==="Spring" && (5.0<=props["Guest Count"] && props["Guest Count"]<=8.0)) { // 4
    return {"Dish":"Steak"};
  }
  else if ((props["Season"]==="Fall" || props["Season"]==="Winter" || props["Season"]==="Spring") && props["Guest Count"] > 8.0) { // 5
    return {"Dish":"Stew"};
  }
  else if (props["Season"]==="Summer") { // 6
    return {"Dish":"Light Salad and a nice Steak"};
    // Hey, why not?
  }
}

