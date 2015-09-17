package powerful;

import java.util.List;


public class Loop {
    public int numberOfOlderResidents(List<Resident> residents, List<Residence> residences) {
        int numberOfOldTimers = 0;
        for (Resident res: residents) {
            for (Residence resc: residences) {
                if (res.getResidentId().equals(resc.getId()) && res.getYearsOfResidence().equals(resc.getAge())) {
                    ++numberOfOldTimers;
                }
            }
        }
        return numberOfOldTimers;
    }
}
