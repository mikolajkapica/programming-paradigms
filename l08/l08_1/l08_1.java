package l08_1;
import java.util.List;

import java.util.ArrayList;

/* Zdefiniuj hierarchię klas dla Pudeł. Każdy rodzaj pudła posiada swoje własne parametry 
np. wymiary prostopadłościanu, czy długość boku czworościanu. Zdefiniuj operacje 
potrzebne do ułożenia pudeł zgodnie z historyjką. Dodatkowo przeciąż metodę 
toString, aby wyświetlała informacje o pudełku. Pamiętaj, że w ogólności „pudło” jako 
taka nie istnieje, (20 pkt.)
*/

abstract class Box {
    private double innerCuboidVolume;
    private double outerCuboidVolume;
    private double volume;

    public Box(double innerCuboidVolume, double outerCuboidVolume, double volume) {
        this.innerCuboidVolume = innerCuboidVolume;
        this.outerCuboidVolume = outerCuboidVolume;
        this.volume = volume;
    }

    public double getInnerCuboidVolume() {
        return innerCuboidVolume;
    }

    public double getOuterCuboidVolume() {
        return outerCuboidVolume;
    }

    public double getVolume() {
        return volume;
    }

    public boolean canContain(Box box) {
        return box.getOuterCuboidVolume() < innerCuboidVolume;
    }

    public String toString() {
        return "Box: Volume=" + volume + " InnerCuboidVolume=" + innerCuboidVolume + " outerCuboidVolume=" + outerCuboidVolume;
    }
}

class CylinderBox extends Box {
    public double radius;
    public double height;

    public CylinderBox(double radius, double height) {
        // double innerSide = 2 * radius / Math.sqrt(2);           |  2r = a * sqrt(2)
        // double outerSide = 2 * radius;                          |  2r = a
        // double innerCuboidVolume = Math.pow(innerSide, 2) * height;   |  a^2 * h
        // double outerCuboidVolume = Math.pow(outerSide, 2) * height;   |  a^2 * h
        // double volume = Math.PI * Math.pow(radius, 2) * height;       |  pi * r^2 * h
        super(Math.pow((2 * radius / Math.sqrt(2)), 2) * height, Math.pow(2 * radius, 2) * height, Math.PI * Math.pow(radius, 2) * height);

        this.radius = radius;
        this.height = height;
    }

    public String toString() {
        return super.toString() + " >: CylinderBox: radius=" + radius + " height=" + height;
    }
}

class CuboidBox extends Box {
    public double baseArea;
    public double height;

    public CuboidBox(double baseArea, double height) {
        super(baseArea * height, baseArea * height, baseArea * height);

        this.baseArea = baseArea;
        this.height = height;
    }

    public String toString() {
        return super.toString() + " >: CuboidBox: baseArea=" + baseArea + " height=" + height;
    }
}

class DeltoidBaseBox extends CuboidBox {
    public double d1;
    public double d2;
    public double h;

    public DeltoidBaseBox(double d1, double d2, double h) {
        super((d1 + d2) / 2, h);

        this.d1 = d1;
        this.d2 = d2;
        this.h = h;
    }

    public String toString() {
        return super.toString() + " >: DeltoidBox: " + d1 + " " + d2 + " " + h;
    }
}

class TriangleBaseBox extends CuboidBox {
    public double a;
    public double h;
    public double H;

    public TriangleBaseBox(double a, double h, double H) {
        super((a * h) / 2, H);

        this.a = a;
        this.h = h;
        this.H = H;
    }

    public String toString() {
        return super.toString() + " >: TriangleBaseBox: a=" + a + " h=" + h + " H=" + H;
    }
}

/*
Zdefiniuj klasę reprezentującą elfa układającego pudła. Jego implementacja ma 
zawierać metodę pozwalającą na ułożenie pudeł z podanej listy. Elf ma zawierać listę 
list pudeł. Każda podlista ma zawierać kolejne pudełka w porządku ich wsadzania. 
Zdefiniuj metody, które umożliwią odczytanie listy list pudeł oraz obliczenie 
całkowitego miejsca jakie te pudła będą zajmować w magazynie,* 
 */


class Elf {
    private List<List<Box>> boxes;

    public Elf() {
        boxes = new ArrayList<List<Box>>();
    }

    public void addBoxes(List<Box> boxes) {
        boxes.sort((b1, b2) -> (int)(b2.getVolume() - b1.getVolume()));
        for (Box box : boxes) {
            addBox(box);
        }
    }

    public void addBox(Box box) {
        if (boxes.size() == 0) {
            List<Box> newBox = new ArrayList<Box>();
            newBox.add(box);
            boxes.add(newBox);
        } else {
            for (List<Box> boxList : boxes) {
                if (boxList.get(boxList.size() - 1).canContain(box)) {
                    boxList.add(box);
                    return;
                }
            }

            List<Box> newBox = new ArrayList<Box>();
            newBox.add(box);
            boxes.add(newBox);
        }
    }

    public List<List<Box>> getBoxes() {
        return boxes;
    }

    public String toString() {
        StringBuilder result = new StringBuilder();

        result.append("Elf boxes:\n");

        for (int i = 0; i < this.getBoxes().size(); i++) {
            result.append("Box list [" + i + "]:");
            for (Box box : this.getBoxes().get(i)) {
                result.append(box.toString() + "\n");
            }
            result.append("\n");
        }

        result.append("Total volume: " + this.getTotalVolume());

        return result.toString();
    }

    public double getTotalVolume() {
        double totalVolume = 0;

        for (List<Box> boxList : boxes) {
            totalVolume += boxList.get(0).getOuterCuboidVolume();
        }

        return totalVolume;
    }
}

public class l08_1 {
    public static void main(String[] args) {
        System.out.println("l08_1 --------------------");

        List<Box> boxes = new ArrayList<Box>();
        boxes.add(new CylinderBox(1, 2));
        boxes.add(new CylinderBox(2, 3));
        boxes.add(new CuboidBox(1, 2));
        boxes.add(new CuboidBox(2, 3));
        boxes.add(new DeltoidBaseBox(1, 2, 3));
        boxes.add(new DeltoidBaseBox(2, 3, 4));
        boxes.add(new TriangleBaseBox(1, 2, 1));
        boxes.add(new TriangleBaseBox(2, 3, 4));

        Elf elf = new Elf();
        elf.addBoxes(boxes);;

        System.out.println(elf.toString());
    }
}