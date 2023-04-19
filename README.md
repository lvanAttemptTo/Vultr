# Vultr

## Overview
Vultr is an application for birders. It allows you to get information on birds, find information about an area, view places where a bird has been sighted, and even quiz yourself on the area you are in.

Vultr Uses Two external APIs [ebird](https://ebird.org/home), and  [Flickr](https://flickr.com). The [ebird API key](https://ebird.org/api/keygen) will need to be supplied while the Flicr key is supplied automatically.

## Functionality

### Species Information

The _Species Information_ tab is for the general information on the species. This includes:
* Species Info
  - Common Name
  - Scientific Name
  - Family
  - Distance of Closest Sighting
  - Location of Closest Sighting
  - Number of Sightings In x Range In The Past x Days
* Sighting Locations
* Similar Species In Area

### Species List

The _Species List_ tab shows the birds sighted in the desired area either in the past x days or ever. The options for area are:
* County
* State
* Country

### Sighting Map

The _Sighting Map_ tab contains a map of sightings of the searched bird in the past x days from the set radius.

### Quiz

The _Quiz_ tab has a quiz on the birds in the selected area. It uses the same list of birds as the  [_Species List_](README.md#species-list) tab. The quiz shows a picture and you have 5 options of birds. This is still in very early development and might be a bit buggy.

### Settings

The _Settings_ tab allows you to set your location including: country, state/providence ect., and county (if offered). you can also set your specific location using a map which will be used in calculating the closest sightings. In the main 48 states of the US the map can be used to set the other location information as well. This is also where you supply the [ebird API key](https://ebird.org/api/keygen). You are also able to set the search radius for the species as well as how far back you want to lookup for the [_Species List_](README.md#species-list), [_Species Information_](README.md#species-information), and [_Species Map_](README.md#sighting-map) tabs.
