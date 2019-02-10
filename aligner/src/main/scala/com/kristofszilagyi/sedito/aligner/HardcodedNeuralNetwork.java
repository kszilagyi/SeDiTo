package com.kristofszilagyi.sedito.aligner;
import smile.classification.LoadableNeuralNetwork;

final public class HardcodedNeuralNetwork {
  public static final LoadableNeuralNetwork nn = new LoadableNeuralNetwork(95, new double[][][]{
    new double[][]{{-0.18634368880950744, -0.012244247067994122, -1.886491227886612, 0.5227679607776367, 0.4019825213343451, -0.3083439778921783, -0.23345699826788116, -4.427421112846308, 3.4232916090979657, -2.359062310125069, 0.12009179666114157, -1.5918382434730505, -0.34389673472368537, -0.3188840092519057, -0.13039129264342592, 0.5919874407010789, 0.14048192577573093, -1.505821207047515, -0.8177943609893773, -0.5636030481906115, -0.8238328488800211, -0.8567821740393038, -0.8580046137137749, 0.5257803521919431, 0.25400735765255034, 0.15473986743631327, -1.3652483098203907, -0.7170626499052675, -1.42414541971735, -1.1317354519134364, 0.5634771084299514, 0.785229530075325, -2.541270001587578, -0.08192007457403469, -0.39496842805362736, -0.8286181036970959, -1.5958602129042239, -0.40535875595940196, 0.19188122992901702, 0.1525378094872106, -0.9763194339696251, -0.36016350207743686, -1.1577888783647585, -1.0341986568250723, -1.4148337468790013, -1.933063801600978, -1.9140155380018546, 0.1310905280676025, -0.9893514455618053, -1.1058775979412172, -0.38081713050321475, 0.09642324880646465, -1.748466255558579, 0.4313477096302342, -0.1320771702195723, -0.3186845122529662, -0.19378018096594576, -1.6585123239784412, -0.04101122830704203, -0.029516109273716916, -0.4639988298905388, -0.00825793267967828, -1.6849388127424927, -0.3225002355346596, -0.8563417383110293, -0.4172957044237421, -0.36720038579041925, -1.5404787813456928, 0.8127539492761561, 0.6029439073431114, -1.3336056845558613, -1.1420129069499385, 3.130529930233, -0.43843898565671263, -1.6809093445628787, -0.4919837454274148, 2.3654980437689854, 1.1100049826208551, 4.240553895303688, 2.282931462480506, 0.8833004365343571, -0.5301121916362275, -0.10670661412888552, 2.7625123532336247, -1.2158771219895796, 0.23695116198232374, 0.8535613199955169, 1.8988034739063289, -1.6277106158419634, 2.130429610219061, 1.0418748701622575, 2.478412573245421, -0.8717499313433715, 1.4014308574576222, 8.285824992163539, -2.170675012562465}, {0.18265861194769314, 0.377172258874841, 3.00282477383444, -3.5454974848488203, -0.5617499974223481, -1.6179222372581012, -1.4704900738722047, 4.97148225813844E-4, -0.08890745933283521, 2.9269848275708217, -2.267426740132422, 1.1601634949925919, 0.6051743164102843, -0.7943585264027154, -0.9686991502898488, -1.0098922852030978, -0.6494830156860697, 1.4208853938194368, -1.7863500685281835, -2.3023039757863426, 1.2517171296940985, 1.6336005608738497, -0.6227920793920736, -0.5684727368216076, -0.6633157160069602, 1.3861846964141653, 2.6851286995241117, -1.3650579946096324, -0.9061777487148026, -1.1925799601483176, 0.4262835748587421, 0.49948382681788644, 0.515174516245304, -1.0324674761479153, -1.1066404075752596, -0.32510156659104744, 0.043033810764070035, 1.0200287250510145, -0.794882636286799, -0.848900273141948, 0.9377593004052666, 0.9174495414913777, -0.1477797398796109, -0.11147621017784211, -0.2135430355598525, 0.6274092406305337, 0.7777979167779778, 0.042975646509023586, -1.2309846821536103, -1.2976667214890232, 0.7143380145069684, 0.771824485045447, 0.03456851310046339, -0.9039135024354457, -0.8358421641317237, -0.19338029030240436, -0.26353203909234674, 0.9876749399509867, -0.10014450066335129, -0.17722602438998739, -0.21555363927357393, -0.08030264731145645, 0.8996984141805001, 0.520955401691632, 0.3971969100742203, 0.3982708939420333, 0.48842289194121036, 0.4564205782067907, 0.6345009883941616, 0.6926160168074406, -1.1220517643941823, -0.6281638509407258, 1.0330891969867413, -1.3263275666309269, 1.0474708229453757, -2.43543656799903, -1.0818322868492292, -0.1478612376891062, 1.93715569851308, -2.4113433991144375, 0.03974400394987391, -1.8470968489180752, 0.6558259817818188, -2.4560723369838597, 0.49996525033514705, -2.7897038706790602, 0.6208286516933396, -3.2550003806386325, -0.20798066542245597, -2.903137725675849, 1.9383407160853203, -3.98705226229515, -0.8654722015943394, -3.399395559080699, -0.3950572694276738, 0.8713086839643305}, {0.9922510302867525, 0.9279083574270403, -0.5141667300534617, -1.6807236540926584, 1.1502398276245314, 0.10888574537949017, 0.06787005983121752, -3.266050773587042, 3.1643126389400225, -0.4675338378849745, 0.14635804915205178, -0.20856548439741, -1.9456407926485029, 4.233283354373674, 3.7988728105244207, 0.1938142206404731, -0.7776063046188546, -1.2671280826208098, -0.8719594067059525, 0.28526643372481003, -0.8313932393255615, -0.3990794146716793, -1.6399062528634574, 0.8833578244794642, 0.7481571440590105, -0.708075737698311, -1.2420938828794066, -0.8117979535823601, 0.8975584931327436, 1.2632701718813621, -2.047307090770427, -1.6790717190238715, -0.3873071125494139, -2.4440788144754264, -2.591762448948518, -0.17889171797800388, 0.3152931504571462, -2.365983786127204, 1.5587403336275354, 1.1725290335853917, -1.1995551540637563, -0.9228994091258755, -1.259217195990496, -1.2847254074891272, -1.5516935402458538, -0.4378126143723753, -0.0976380588823733, -1.9658276386773492, 3.00482392766008, 2.8156313100912613, -1.7300816622433561, -1.3740272581917783, -0.763233444501664, -0.26327742240879887, -0.477114070690593, -0.7429837761095408, -0.4184745874437105, -1.6286786414758214, 1.4023886416023217, 1.0971364331668858, -0.679000923806324, -0.5136079686437067, -1.7014353525584343, 0.5457717312956614, 0.3661975551246503, -0.5401215124190509, -0.2847180881760256, -1.7781589210807212, 1.0466592845721223, 0.8567124711835419, 3.1505528813054906, -1.1716901029167794, 0.44828781064753104, -0.12298864018928757, 1.300194227487115, -0.698473745580007, -3.0384781514763346, 1.4644048159978262, 1.2574751899894625, 0.17600246490258703, -0.5742435272287886, 0.8457085474308689, 1.3840324899251324, 0.049506573787523454, -0.6539453830682076, 0.9987270345777888, 1.2458729545973912, -2.4536859084735276, -0.6329993231461536, -1.4883089704604209, 0.9739437762455114, 1.2188331699162935, -1.0866016070689042, 2.350959075252381, 7.569247487953016, -2.321755316885193}, {0.03873665880861085, -0.03539674370032478, 1.8229012763471584, -0.30773927929902195, -0.7025333622151894, -1.8540655173732816, -1.827776678708471, 0.96356883129616, -1.1518879699728677, 1.6725677892579596, -2.551220523772881, -1.4328285916514305, 4.005728097736463, 0.47799551497680975, 0.05620301827256342, 1.4514534982279337, 1.5810121392441754, -0.030273187951518607, 0.5354489193539264, -0.5800481178589489, -0.2851426752861805, -0.15185610385038587, 2.563216724771969, 1.111941295551537, 0.7279986780465745, 2.3744247071442453, 2.083287533491226, -0.4122702176085975, 1.441439106900019, 0.8941729019907863, -0.17126128858837586, 0.03301131089409738, 2.42863023915107, 0.3277605552329278, 0.21239672746660748, 2.5062241059544657, 1.0339885523791592, 0.8033686011996322, 0.3480398677472845, 0.8930379494947989, 0.7206364872659198, 0.5689485598317396, 1.8563170963322162, -1.1313222554628575, -1.0819593974574617, 1.538642497476153, 0.0772961390821406, 1.720087107070385, -2.219115387758034, -1.801998444281938, 1.1584818668358778, 1.0880033387606964, 1.3612691421677727, -0.5740585459205705, -0.4725162132879044, 1.387212704018563, 0.031369064056470136, 1.725861967943369, -0.5530611714556923, 0.061073659982015284, 0.3178871390015066, 0.2094794105867253, 2.312467109688798, -0.40667123126520677, -0.3697563469545698, 1.8624384247004142, 0.7955850237216135, 1.1298839913012848, -1.0462505924197427, -0.5574745095806766, -4.0423939811705445, 0.16423500819953168, -2.058902385763568, -0.35508758741046853, 0.762095325701514, -1.2583610040357165, 2.5870549499701827, -1.4162508616543494, -4.336626203426061, -1.7997876541608306, 0.97459855156887, -0.7572057619479916, 1.3091283594338936, 0.6071913687734203, 0.20771868920756406, -0.44542287405701775, 1.5862261923235332, -0.5325272177130024, 0.11562952299590647, -1.6947606586064266, 0.32680632365684326, 1.272824481920834, -0.3688937286577956, 1.3223247681957608, -6.335983880208594, 2.668709462260788}, {-0.8120363431827256, -0.8880546180068174, 1.7840752380880567, -1.335663305135891, -0.8443003428117524, -3.845665431242598, -3.892149167934321, 6.496766231548575, -0.8774352867543596, 0.7920573749347101, -2.590039587027725, -0.13580323154242488, 1.4072949667212296, -0.5184976148720066, -0.48494492977386555, -4.060923399111725, -1.0978785566311438, 0.8264676674653026, 0.9103111244752985, 0.5634553975141174, -0.29893977733036015, 0.16684904918250773, 0.3311426445647481, -0.48069306864534755, -0.46509772691546186, -2.1195991562156125, 0.8282054128165777, -0.9396523492189083, 0.5689838704790459, 0.38781964827740156, 0.26606789039094425, 0.40055226088503326, -0.1790130917148231, 0.036642644828641095, 0.0071977270025672, -1.916760013972855, -1.070138576128307, 0.4077375468415917, 0.709731898872311, 1.1887949977574235, -0.4599137744727173, -0.36104337132107495, 0.6699288712871825, -1.5651225724841082, -1.5512181393143392, 0.7706052946999634, 0.4829436110350371, -1.318050172440275, -1.336918484837411, -0.4878575347691426, -0.2085520506765635, -0.25162848375219177, 0.40886923714915624, 0.49599603681303883, 0.3650186807054718, -0.49301485314498195, -1.1743344413715262, 0.19380180214931972, -1.1105450117655284, -0.45222981838066073, -0.26855984766046853, -0.36737945250566206, 0.6203920119079295, -0.7548874147926552, -0.765495090097779, 0.3767978342468297, -0.6191486997226469, -0.5977160878055927, -0.46495861238272335, 0.37830549969412897, -2.068133179712176, -0.3028011903053059, -0.44871639723574347, 1.4584782323877548, -3.442146488875721, 0.8580805280191678, 1.0107631464147668, -0.7151647265509951, -0.8771243259696762, -0.47886448673276033, 0.6875777790718507, -0.14046230691691733, -1.1584112497491608, 1.6718679770476754, -0.1621855019212184, 0.9176128849166284, -2.7083617333791397, 2.1416804495631085, -0.21811981471084652, 1.6160743808283138, -2.974729036658081, 1.6282167210350318, -2.662352433363618, 0.7495038113229472, -0.947203115711529, 0.1860991080609563}},
    new double[][]{{-3.464900780921107, 5.5345563269187465, -4.962778872544548, 3.622755275650212, 5.571175155356271, -2.4889863638590963}}
  });
}
       