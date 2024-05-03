# asthma ATC codes
atc_codes_saba <- c(
    "R03AC02", # salbutamol (short-acting beta-2 agonist)
    "R03AC03", # terbutalin (short-acting beta-2 agonist)
    "R03CC02", # salbutamol (short-acting beta-2 agonist, oral)
    "R03CC03" # terbutalin (short-acting beta-2 agonist, oral)
)
atc_codes_laba <- c(
    "R03AC12", # salmeterol (long-acting beta-2 agonist)
    "R03AC13", # formoterol (long-acting beta-2 agonist)
    "R03CC12" # bambuterol (long-acting beta-2 agonist, oral)
)
atc_codes_ics_laba <- c(
    "R03AK06", # salmeterol (long-acting beta-2 agonist) + fluticasone (corticosteroid)
    "R03AK07", # formoterol (long-acting beta-2 agonist) + budesonide (corticosteroid)
    "R03AK08" # formoterol (long-acting beta-2 agonist) + beclometasone (corticosteroid
)
atc_codes_ics <- c(
    "R03BA01", # beclometasone (corticosteroid)
    "R03BA02", # budesonide (corticosteroid)
    "R03BA05", # fluticasone (corticosteroid)
    "R03BA08" # ciclesonide (corticosteroid)
)
atc_codes_ltra <- c(
    "R03DC03" # montelukast (leukotriene receptor antagonist)

)
atc_codes_lama <- c(
    "R03BB04" # tiotropium (bromide) (long-acting muscarinic antagonist)
)
atc_codes_sama <- c(
    "R03BB01" # ipratropium (bromide) (short-acting muscarinic antagonist)
)
atc_codes_other_asthma_medications <- c(
    "R03BC01", # cromoglicic acid (mast cell stabilizer)
    "R03DA02" # choline theophyllinate (cough medicine acting as a bronchodilator)
)
# combine the atc codes into one vector, with the variable name asthma_atc_codes
asthma_atc_codes <- c(atc_codes_saba, atc_codes_laba, atc_codes_ics_laba, atc_codes_ics, atc_codes_ltra, atc_codes_lama, atc_codes_sama, atc_codes_other_asthma_medications)



# eczema ATC codes
atc_codes_basic_ointments <- c(
    "D02AE01", # carbamid / urea
    "D02AX"  # other "skin-protective and emollient preparations" (i.a., glycerol [Miniderm])
)
atc_codes_group_i_corticosteroids <- c(
    "D07AA", # group I corticosteroids
    "D07XA01" # hydrocortisone (corticosteroid – I) + NaCl + urea
)
atc_codes_moderate_strong_corticosteroids <- c(
    "D07AB", # group II corticosteroids
    "D07AC", # group III corticosteroids
    "D07AD", # group IV corticosteroids
    "D07BB03", # triamcinolone (corticosteroid – II) + antiseptics
    "D07BC01", # betamethasone (corticosteroid – III) + antiseptics
    "D07XB02", # triamcinolone (corticosteroid – II) + salicylic acid
    "D07XC01", # betamethasone (corticosteroid – III) + salicylic acid
    "D07XC03" # mometasone (corticosteroid – III)
)
atc_codes_dermatologic_immunosuppresants <- c(
    "D11AH01", # tacrolimus (immunosuppressant)
    "D11AH02" # pimecrolimus (immunosuppressant)
)
eczema_atc_codes <- c(atc_codes_basic_ointments, atc_codes_group_i_corticosteroids, atc_codes_moderate_strong_corticosteroids, atc_codes_dermatologic_immunosuppresants)



# rhinitis ATC codes
atc_codes_nasal_steroids <- c(
    "R01AD" # corticosteroids (nasal sprays)
)
atc_codes_antihistamines_and_mast_cell_stabilizers <- c(
    "R01AC01", # cromoglic acid (mast cell stabilizer) (nasal)
    "R01AC02", # levocabastine (antihistamine) (nasal)
    "R06A", # systemic antihistamines
    "S01GX01", # cromoglicic acid (mast cell stabilizer) (ophthalmological)
    "S01GX02", # levocabastine (antihistamine) (ophthalmological)
    "S01GX04", # nedocromil (mast cell stabilizer) (ophthalmological)
    "S01GX06", # emedastine (antihistamine) (ophthalmological)
    "S01GX08", # ketotifene (antihistamine) (ophthalmological)
    "S01GX09" # olopatadine (antihistamine) (ophthalmological)
)
# "R01AX10", # other nasal preparations (n=2, one nasal ointment for nasal bruises, one soothing nasal congestion relief preparation, which can be used for colds in addition to catarrh, and hayfever – a bit too unspecific)
rhinitis_atc_codes <- c(atc_codes_nasal_steroids, atc_codes_antihistamines_and_mast_cell_stabilizers)