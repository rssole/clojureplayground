class Loop
  def number_of_older_residents(residents, residences)
    old_timers = (residents * residences).select do |res, resc|
      res.residence_id == resc.id && res.years_of_residence > resc.age
    end
    number_of_old_timers = old_timers.size
  end
end