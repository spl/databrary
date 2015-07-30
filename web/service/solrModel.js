'use strict';

app.factory('solrModelService', [
  '$q', '$cacheFactory', '$play', 'routerService', 'constantService', 'Segment',
  function ($q, $cacheFactory, $play, router, constants, Segment) {

    // ---------------------------- SolrModel - base class
    function SolrModel(init) {
      this.init(init);
    }

    SolrModel.prototype.init = function (init) {
      var fields = this.fields;
      init = solrFieldRewrite(init);
      for (var f in fields) {
        if (f in init)
          this[f] = init[f];
        else if (fields[f])
          delete this[f];
      }
      // return(init);
    };

    // ---------------------------- SolrSlot
    function SolrSlot(init) {
      SolrModel.call(this, init);
    }

    SolrSlot.prototype.fields = {
      release: false,
      segment: true,
      tags: false,
      releases: false,
      gender: false,
      race: false,
      age: false,
      ethnicity: false,
      text: false,
      containerId: true,
      volumeId: true,
      recordId: true
    };

    SolrSlot.prototype.init = function (init) {
      SolrModel.prototype.init.call(this, init);
    };

    // ---------------------------- SolrVolume
    function SolrVolume(init) {
      // this.init(init);
      SolrModel.call(this, init);
    }

    SolrVolume.prototype.fields = {
      id: true,
      permission: false,
      name: true,
      alias: true,
      body: true,
      doi: true,
      creation: true,
      owners: true,
      ownerIds: true,
      citation: false,
      links: false,
      funding: false,
      tags: false,
    };

    SolrVolume.prototype.init = function (init) {
      SolrModel.prototype.init.call(this, init);
    };

    // ---------------------------- SolrParty
    function SolrParty(init) {
      // this.init(init);
      SolrModel.call(this, init);
    }

    SolrParty.prototype.fields = {
      id: true,
      permission: false,
      name: true,
      sortname: true,
      prename: true,
      orcid: true,
      affiliation: true,
      email: true,
      institution: true,
      url: true,
      authorization: false,
    };

    SolrParty.prototype.init = function (init) {
      SolrModel.prototype.init.call(this, init);
    };

    function solrFieldRewrite(solrVals) {
      // Rewrite the keys in init to the things we expect here
      var solrKeyNames = {
        "party_pre_name_s" : "prename",
        "party_name_s": "sortname",
        "party_affiliation_s": "affiliation",
        "party_id_i": "id",
        "volume_id_i": "id",
        "title_t": "name",
        "abs_t": "body",
        "citation_t": "citation",
        "volume_tags_ss": "tags",
        "alias_s": "alias",
        "volume_keywords_ss": "keywords",
        "volume_owner_names_ss": "owners",
        "volume_owner_ids_is": "ownerIds",
        "record_id_i": "recordId",
        "record_container_i": "containerId",
        "record_ethnicity_s": "ethnicity",
        "record_gender_s": "gender",
        "record_race_s": "race",
        "record_age_td": "age",
        "record_segment_s": "segment",
        "record_text_t": "text",
      };

      // Rewrite solrVals to use the solrKeynames names

      var solrValsRewrite = Array();
      for (var k in solrVals) {
        var v = solrVals[k];
        var rewriteName = solrKeyNames[k];
        solrValsRewrite[rewriteName] = v;
      }

      if(solrValsRewrite.prename !== undefined) {
        solrValsRewrite.name = solrValsRewrite.prename + " " + solrValsRewrite.sortname;
      } else if (solrValsRewrite.party_id_i !== undefined) {
        solrValsRewrite.name = solrValsRewrite.sortname;
      }

      return solrValsRewrite;
    }

    function solrPartyPeek(id) {
    }

    function solrPartyMake(init) {
      return new SolrParty(init);
    }

    function solrPartyMakeSubArray(l) {
      for (var i = 0; i < l.length; i ++)
        l[i].solrParty = solrPartyMake(l[i].solrParty);
      return l;
    }

    function solrPartyMakeArray(l) {
      if (l) for (var i = 0; i < l.length; i ++)
        l[i] = solrPartyMake(l[i]);
      return l;
    }

    function solrPartyGet(id, p, options) {
      return solrPartyMake(p);
    }

    SolrParty.get = function (id, options) {
      return solrPartyGet(id, solrPartyPeek(id), options);
    };

    SolrParty.prototype.get = function (options) {
      return solrPartyGet(this.id, this, options);
    };

    SolrParty.prototype.route = function () {
      return router.solrParty([this.id]);
    };

    Object.defineProperty(SolrParty.prototype, 'lastName', {
      get: function () {
        return this.name.substr(this.name.lastIndexOf(' ')+1);
      }
    });

    SolrParty.prototype.avatarRoute = function (size, nonce) {
      var params = {};
      if (nonce)
        params.nonce = nonce;

      return router.partyAvatar([this.id, size || 56], params);
    };

    return {
      SolrParty: SolrParty,
      SolrVolume: SolrVolume
    };
  }

]);
