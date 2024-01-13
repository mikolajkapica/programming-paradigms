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

    public Box(double innerCuboidVolume, double outerCuboidVolume) {
        this.innerCuboidVolume = innerCuboidVolume;
        this.outerCuboidVolume = outerCuboidVolume;
    }

    public double getInnerCuboidVolume() {
        return innerCuboidVolume;
    }

    public double getOuterCuboidVolume() {
        return outerCuboidVolume;
    }

    public boolean canContain(Box box) {
        return box.getOuterCuboidVolume() <= innerCuboidVolume;
    }

    public String toString() {
        return "Box: " + innerCuboidVolume + " " + outerCuboidVolume;
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
        super(Math.pow((2 * radius / Math.sqrt(2)), 2) * height, Math.pow(2 * radius, 2) * height);

        this.radius = radius;
        this.height = height;
    }

    public String toString() {
        return super.toString() + " subclass of CylinderBox: radius=" + radius + " height=" + height;
    }
}

class CuboidBox extends Box {
    public double baseArea;
    public double height;

    public CuboidBox(double baseArea, double height) {
        super(baseArea * height, baseArea * height);

        this.baseArea = baseArea;
        this.height = height;
    }

    public String toString() {
        return super.toString() + " subclass of CuboidBox: baseArea=" + baseArea + " height=" + height;
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

    public TriangleBaseBox(double a, double h) {
        super(a * h / 2, h);

        this.a = a;
        this.h = h;
    }

    public String toString() {
        return super.toString() + " >: TriangleBaseBox: a=" + a + " h=" + h;
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

    public void addBox(Box box) {
        if (boxes.size() == 0) {
            List<Box> newBox = new ArrayList<Box>();
            newBox.add(box);
            boxes.add(newBox);
        } else {
            for (List<Box> boxList : boxes) {
                if (boxList.get(0).canContain(box)) {
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

        for (List<Box> boxList : boxes) {
            for (Box box : boxList) {
                result.append(box.toString() + "\n");
            }
        }

        return result.toString();
    }

    public double getTotalVolume() {
        double totalVolume = 0;

        for (List<Box> boxList : boxes) {
            for (Box box : boxList) {
                totalVolume += box.getOuterCuboidVolume();
            }
        }

        return totalVolume;
    }
}

