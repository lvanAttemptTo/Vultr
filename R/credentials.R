cookie_expiry <- 7

# This function must return a data.frame with columns user and sessionid.  Other columns are also okay
# and will be made available to the app after log in.

get_sessions_from_db <- function(conn = db, expiry = cookie_expiry) {
    dbReadTable(conn, "sessions") %>%
        mutate(login_time = ymd_hms(login_time)) %>%
        as_tibble() %>%
        filter(login_time > now() - days(expiry))
}

# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password.

add_session_to_db <- function(user, sessionid, conn = db) {
    tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
        dbWriteTable(conn, "sessions", ., append = TRUE)
}

db <- dbConnect(SQLite(), ":memory:")
dbCreateTable(db, "sessions", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))

user_base <- tibble(
    user = c("lvan", "abc"),
    password = c("test", "pass"),
    password_hash = sapply(c("test", "pass"), sodium::password_store),
    permissions = c("admin", "standard"),
    name = c("Luke VanDeGrift", "ABC"),
    country = c("United States", "United Kingdom"),
    state = c("Oregon", "England"),
    county = c("Multnomah", "Bedfordshire"),
    ebirdKey = c ("vmgu1o6c6ihc", "vmgu1o6c6ihc"),
    specificLocation = c(TRUE, FALSE),
    specificLatitude = c("45.5896568645855", ""),
    specificLongitude = c("-122.738592624664", ""),
    radius = c(25, 10),
    daysBack = c(7, 30),
    lifeList = c("Merlin;Rufous Hummingbird;Hutton's Vireo;Lesser Yellowlegs;Rhinoceros Auklet;Dunlin;Sanderling;Black Turnstone;Brant;White-winged Scoter;Surf Scoter;Long-tailed Duck;Pigeon Guillemot;Red-necked Grebe;Barrow's Goldeneye;Pileated Woodpecker;Rough-legged Hawk;Short-eared Owl;Hooded Merganser;Western Bluebird;Red-crowned Parrot;Acorn Woodpecker;Blue-gray Gnatcatcher;Sharp-shinned Hawk;Black-necked Stilt;Greater Roadrunner;Clark's Grebe;Yellow-crowned Night-Heron;Ridgway's Rail;Lincoln's Sparrow;Cassin's Kingbird;American Pipit;Whimbrel;Wrentit;Say's Phoebe;Royal Tern;Forster's Tern;Caspian Tern;Short-billed Gull;Willet;Long-billed Curlew;White-faced Ibis;Common Gallinule;Hermit Thrush;House Wren;Red-shouldered Hawk;Orange-crowned Warbler;California Towhee;Black Phoebe;Allen's Hummingbird;Snowy Egret;Pelagic Cormorant;Brandt's Cormorant;Heermann's Gull;Common Murre;Harlequin Duck;Brown Pelican;Purple Finch;Band-tailed Pigeon;Western Gull;Glaucous-winged Gull;Pacific Wren;Pacific-slope Flycatcher;Lazuli Bunting;Yellow-breasted Chat;Swainson's Thrush;Spotted Sandpiper;Horned Lark;Cassin's Finch;Trumpeter Swan;Broad-tailed Hummingbird;MacGillivray's Warbler;Dusky Flycatcher;Willow Flycatcher;Western Wood-Pewee;Yellow Warbler;Mountain Bluebird;Mountain Chickadee;Clark's Nutcracker;White-throated Swift;Eared Grebe;Great Horned Owl;Gray Catbird;Brown Thrasher;Green Heron;Herring Gull;Ring-billed Gull;Blue Jay;Bobolink;Cooper's Hawk;Baltimore Oriole;Dickcissel;Northern Cardinal;Chipping Sparrow;Chimney Swift;Common Grackle;Brown-headed Cowbird;Western Meadowlark;Common Nighthawk;Common Raven;Western Kingbird;Black-billed Magpie;Swainson's Hawk;Eastern Kingbird;Prairie Falcon;Ferruginous Hawk;California Gull;Eurasian Collared-Dove;California Quail;Warbling Vireo;Black-headed Grosbeak;Vaux's Swift;Wilson's Warbler;Western Tanager;Yellow-headed Blackbird;Savannah Sparrow;Marsh Wren;Northern Rough-winged Swallow;Blue-winged Teal;Yellow-rumped Warbler;Common Yellowthroat;Greater Yellowlegs;Pied-billed Grebe;Cliff Swallow;Barn Swallow;Purple Martin;Violet-green Swallow;Turkey Vulture;Peregrine Falcon;Osprey;Golden Eagle;Red-throated Loon;Greater Scaup;Tufted Duck;Harris's Sparrow;Ring-necked Duck;Canvasback;Snow Goose;Sandhill Crane;Killdeer;Western Grebe;Horned Grebe;Common Goldeneye;Lesser Scaup;Steller's Jay;White-throated Sparrow;Brown Booby;Red Junglefowl;Pacific Golden-Plover;Warbling White-eye;Red-vented Bulbul;Rose-ringed Parakeet;Black-crowned Night-Heron;Zebra Dove;Yellow-fronted Canary;Cattle Egret;Common Waxbill;White Tern;Red-crested Cardinal;Java Sparrow;Rock Pigeon;Common Myna;Spotted Dove;American White Pelican;European Starling;Black-capped Chickadee;American Kestrel;Belted Kingfisher;Wilson's Snipe;Long-billed Dowitcher;American Coot;Ruddy Duck;Common Merganser;Bufflehead;Green-winged Teal;Northern Pintail;Mallard;American Wigeon;Gadwall;Northern Shoveler;Tundra Swan;Brewer's Blackbird;American Robin;Spotted Towhee;Fox Sparrow;American Bittern;Red-breasted Merganser;Eurasian Wigeon;Wood Duck;Pine Siskin;American Crow;Bushtit;Double-crested Cormorant;Cedar Waxwing;Bald Eagle;Cackling Goose;Canada Goose;Dark-eyed Junco;Bewick's Wren;Ruby-crowned Kinglet;Red-breasted Sapsucker;Anna's Hummingbird;Chestnut-backed Chickadee;Northern Flicker;White-breasted Nuthatch;Downy Woodpecker;Golden-crowned Sparrow;White-crowned Sparrow;American Goldfinch;Lesser Goldfinch;House Finch;California Scrub-Jay;Mourning Dove;Golden-crowned Kinglet;Brown Creeper;House Sparrow;Red-winged Blackbird;Bank Swallow;Tree Swallow;Hairy Woodpecker;Great Gray Owl;Red-breasted Nuthatch;Canada Jay;Ruffed Grouse;Song Sparrow;Varied Thrush;Northern Harrier;Great Egret;Great Blue Heron;Cinnamon Teal;Red-tailed Hawk;Indian Peafowl;Mandarin Duck;cormorant sp.;swallow sp.;goose sp.;new world sparrow sp.;Buteo sp.;gull sp.", "House Sparrow")
)


