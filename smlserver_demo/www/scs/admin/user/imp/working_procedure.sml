val user_id = ScsLogin.auth_roles [ScsRole.SiteAdm, ScsRole.ScsPersonAdm]

val title = ScsDict.s [(ScsLang.en,`Central Personnel Register - Working Procedure (Danish only)`),
		       (ScsLang.da,`Centralt Person Register - Vejledning`)]

val content = `

  Design og implementation af det centrale personregister (matrikel)
  er dokumenteret <a href="/ucs/doc/scs/scs_users.html">separat.</a><p>

  Denne vejledning fokuserer på de arbejdsopgaver, som skal
  gennemføres for at det centrale personregister (matrikel) er
  opdateret.<p>

  Vi har tre eksterne kilder: 

  <ul>
    <li> Login fra IT-afdelingen
    <li> Personoplysninger på studerende fra HSAS
    <li> Personoplysninger på personer der er tilknyttet IT-C
    (personfortegnelsen)
  </ul>

  Vi gemmer kun simpel information om hver person i det centrale
  personregister, dvs. navn, cpr, email og url til
  hjemmeside. Derudover kan vi i det centrale personregister se, hvor
  de enkelte personoplysninger kommer fra. En enkelt person i det
  centrale personregister kan stamme fra mere end en ekstern kilde. En
  ansat vil eksempelvis stamme fra personfortegnelsen samt login fra
  IT-afdeligen. En ansat der også er studerende vil stamme fra alle
  tre eksterne kilder.<p>

  Data fra de eksterne kilder overføres til det centrale
  personregister en gang i døgnet. Vi har opstillet nogle regler, som
  gør, at det centrale personregister for langt de fleste personer kan
  opdateres automatisk (cirka 95%). Til de sidste 5% må vi manuelt
  tage stilling til, om en person eksempelvis findes i forvejen eller
  skal oprettes som en ny person. En post indlæses automatisk såfremt
  et af følgende er opfyldt:

  <ol> 

  <li> der er et eksakt match i personfortegnelsen, dvs. enten at 1)
  posten har tidligere været indlæst eller 2) at der findes en person
  med samme email og normaliseret navn eller 3) at der findes en
  person med samme cpr-nummer. Hvis en person har navnet "Hans Peter
  Matiesen", så er det normaliserede navn "hansmatiesen", dvs. det
  første fornavn samt efternavn sammensat med små bogstaver.

  <li> hvis der ikke er et eksakt match, så oprettes personen på ny
  såfremt, at der ikke allerede findes en person med et tilsvarende
  normaliseret navn.

  </ol>

  Vi har udviklet nogle skærmbilleder, som kan anvendes til at
  indsætte de personer, der kræver manuel stillingtagen.<p>

  Skærmbilledet <a href="imp_form.sml">Centralt Person Register</a>
  viser hvilke poster i de tre eksterne kilder, som ikke er blevet
  indlæst. Ved at klikke på linket <b>automatisk indlæsning</b> vil
  systemet forsøge at indlæse posterne efter de regler der er
  beskrevet ovenfor.<p>

  For hver post der ikke er indlæst kan vi ud over data på personen
  (navn, cpr og email) se hvornår posten sidst er forsøgt indlæst
  <b>Sidste Import</b> samt om der findes et <b>eksakt match</b> i det
  centrale personregister. Der findes et eksakt match såfremt at
  systemet kan finde en og kun en person i det centrale
  personregister, som matcher udfra de opstillede regler beskrevet
  ovenfor. Hvis dette er tilfældet vil posten blive indlæst næste gang
  man klikker <b>automatisk indlæsning</b> eller klikker på linket
  <b>import</b>, som vil stå i kolonnen <b>Eksakt Match</b>.<p>

  Såfremt posten ikke kan indlæses automatisk kan vi vælge at se mere
  information om posten (kolonne <b>Mere Info</b>). På skærmbilledet
  <b>Mere Info</b> kan vi vælge 1) at slette posten (således at den
  ikke vil blive en del af det centrale personregister), eller 2) at
  oprette personen som ny eller 3) at indlæse personen som en der
  allerede eksisterer med samme normaliseret navn.

  <h2>Check for Inkonsistente Data</h2>

  Det er muligt at sammenholde personoplysningerne fra det centrale
  personregister med de eksterne kilder. Ved at klikke linket <a
  href="chk_inconsistencies.sml">check for inkonsistente data</a> får
  vi tre tabeller for hver ekstern kilde. Tabellerne viser 

  <ol>

    <li> personer med cpr-nummer i det centrale personregister, som
    ikke matcher det cpr-nummer som findes i den eksterne kilde.

    <li> personer i det centrale personregister med navne, som er
    forskellige fra de der findes i de eksterne kilder
    
    <li> personer i det centrale personregister med emails, som er
    forskellige fra de der findes i de eksterne kilder

  </ol>

    Fejl skal rettes i de eksterne kilder hvorefter det centrale
    personregister efterfølgende vil blive opdateret.<p>

    Du er altid velkommen til at kontakte <a
    href="mailto:^(ScsUserImp.service_adm_email)">^(ScsUserImp.service_adm_email)</a>,
    for hjælp eller ved rapportering af fejl.

`

val _ = ScsUserImp.returnPg title
  (`<h1>^title</h1> 
   ` ^^ content)
