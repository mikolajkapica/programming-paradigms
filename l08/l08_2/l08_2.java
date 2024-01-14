package l08_2;
import java.util.List;

import java.util.ArrayList;

enum BoxType {
    CYLINDER_BOX,
    CUBOID_BOX,
    DELTOID_BASE_BOX,
    TRIANGLE_BASE_BOX
}

/*
 * Parameters:
 * CYLINDER_BOX: radius, height
 * CUBOID_BOX: baseArea, height
 * DELTOID_BASE_BOX: d1, d2, height
 * TRIANGLE_BASE_BOX: a, height
 */
class Box {
    private BoxType type;
    private double innerVolume;
    private double outerVolume;
    private double[] specialParameters;

    Box(BoxType type, double[] specialParameters) {
        this.type = type;
        this.specialParameters = specialParameters;

        switch (type) {
            case CYLINDER_BOX:
                double radius = specialParameters[0];
                double height = specialParameters[1];
                this.innerVolume = Math.pow((2 * radius / Math.sqrt(2)), 2) * height;
                this.outerVolume = Math.pow((2 * radius), 2) * height;
                break;
            case CUBOID_BOX:
                double baseArea = specialParameters[0];
                height = specialParameters[1];
                this.innerVolume = baseArea * height;
                this.outerVolume = baseArea * height;
                break;
            case DELTOID_BASE_BOX:
                double d1 = specialParameters[0];
                double d2 = specialParameters[1];
                height = specialParameters[2];
                this.innerVolume = (d1 + d2) / 2 * height;
                this.outerVolume = (d1 + d2) / 2 * height;
                break;
            case TRIANGLE_BASE_BOX:
                double a = specialParameters[0];
                double h = specialParameters[1];
                double H = specialParameters[2];
                double area = (a * h) / 2;
                this.innerVolume = area * H;
                this.outerVolume = area * H;
                break;
        }
    }

    public double getInnerCuboidVolume() {
        return innerVolume;
    }

    public double getOuterCuboidVolume() {
        return outerVolume;
    }

    public boolean canContain(Box box) {
        return box.getOuterCuboidVolume() < getInnerCuboidVolume();
    }

    public BoxType getType() {
        return type;
    }

    public double[] getSpecialParameters() {
        return specialParameters;
    }

    public String toString() {
        switch (type) {
            case CYLINDER_BOX:
                return "Box: InnerCuboid=" + innerVolume + " OuterCuboid=" + outerVolume + " >: CylinderBox: radius=" + specialParameters[0] + " height=" + specialParameters[1];
            case CUBOID_BOX:
                return "Box: InnerCuboid=" + innerVolume + " OuterCuboid=" + outerVolume + " >: CuboidBox: baseArea=" + specialParameters[0] + " height=" + specialParameters[1];
            case DELTOID_BASE_BOX:
                return "Box: InnerCuboid=" + innerVolume + " OuterCuboid=" + outerVolume + " >: CuboidBox: baseArea=" + specialParameters[0] * specialParameters[1] + " height=" + specialParameters[2] + " DeltoidBox: " + specialParameters[0] + " " + specialParameters[1] + " " + specialParameters[2];
            case TRIANGLE_BASE_BOX:
                return "Box: InnerCuboid=" + innerVolume + " OuterCuboid=" + outerVolume + " >: CuboidBox: baseArea=" + (specialParameters[0] * specialParameters[1]) / 2 + " height=" + specialParameters[2] + " TriangleBox: " + specialParameters[0] + " " + specialParameters[1] + " " + specialParameters[2];
            default:
                throw new IllegalArgumentException("Invalid BoxType");
        }
    }
}

class Elf {
    private List<List<Box>> boxes;

    public Elf() {
        boxes = new ArrayList<List<Box>>();
    }

    public void addBoxes(List<Box> boxes) {
        boxes.sort((b1, b2) -> {
            double avgVolume1 = (b1.getInnerCuboidVolume() + b1.getOuterCuboidVolume()) / 2;
            double avgVolume2 = (b2.getInnerCuboidVolume() + b2.getOuterCuboidVolume()) / 2;
            return (int) -(avgVolume1 - avgVolume2);
        }); 
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
            totalVolume += boxList.get(0).getOuterCuboidVolume();
        }

        return totalVolume;
    }
}


public class l08_2 {
    public static void main(String[] args) {
        System.out.println("l08_2 --------------------");

        List<Box> boxes = new ArrayList<Box>();
        boxes.add(new Box(BoxType.CYLINDER_BOX, new double[] {1, 2}));
        boxes.add(new Box(BoxType.CYLINDER_BOX, new double[] {2, 3}));
        boxes.add(new Box(BoxType.CUBOID_BOX, new double[] {1, 2}));
        boxes.add(new Box(BoxType.CUBOID_BOX, new double[] {2, 3}));
        boxes.add(new Box(BoxType.DELTOID_BASE_BOX, new double[] {1, 2, 3}));
        boxes.add(new Box(BoxType.DELTOID_BASE_BOX, new double[] {2, 3, 4}));
        boxes.add(new Box(BoxType.TRIANGLE_BASE_BOX, new double[] {1, 2, 1}));
        boxes.add(new Box(BoxType.TRIANGLE_BASE_BOX, new double[] {2, 3, 4}));

        Elf elf = new Elf();
        elf.addBoxes(boxes);;;

        for (int i = 0; i < elf.getBoxes().size(); i++) {
            System.out.println("Box list [" + i + "]:");
            for (Box box : elf.getBoxes().get(i)) {
                System.out.println(box.toString());
            }
        }

        System.out.println("Total volume: " + elf.getTotalVolume());
    }
}